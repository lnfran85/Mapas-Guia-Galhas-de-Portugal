# ══════════════════════════════════════════════════════════════════════
# ANÁLISE ESPACIAL + FENOLOGIA (iNaturalist)
# Português Europeu
# Inclui:
# - Distribuição espacial (CRS otimizado para grid_base de imediato)
# - Fenologia Global e por Espécie (Centro com "quesitos" + Anel de meses cinzento)
# ══════════════════════════════════════════════════════════════════════

# -------------------- PACKAGES --------------------
library(sf)
library(dplyr)
library(ggplot2)
library(elevatr)
library(rnaturalearth)
library(rnaturalearthhires)
library(ggtext)
library(viridis)
library(tidyverse)
library(lubridate)
library(stringr)
library(tidyr)
library(spatstat.geom)
library(spatstat.explore)
library(spdep)
library(showtext)
library(sysfonts)
library(ggnewscale) 
library(grid)

font_add_google("Poppins")
showtext::showtext_auto()

# -------------------- CONFIGURAÇÃO LOCAL --------------------
Sys.setlocale("LC_TIME", "pt_PT.UTF-8")

# -------------------- DIRECTÓRIOS --------------------
base_dir <- "Outputs"
dir_sp    <- file.path(base_dir, "Distribuicao_por_especie")
dir_rich  <- file.path(base_dir, "Distribuicao_richness")
dir_feno  <- file.path(base_dir, "Fenologia")
dir_eco   <- file.path(base_dir, "Espacio_ecologico")
dir_bias  <- file.path(base_dir, "Bias_por_especie")

dir.create(base_dir, showWarnings = FALSE)
dir.create(dir_sp,   showWarnings = FALSE)
dir.create(dir_rich, showWarnings = FALSE)
dir.create(dir_feno, showWarnings = FALSE)
dir.create(dir_eco, showWarnings = FALSE)
dir.create(dir_bias, showWarnings = FALSE)

# -------------------- CONFIGURAÇÃO DE ICONOS --------------------
path_inverno   <- "iconos/inverno.png"
path_primavera <- "iconos/primavera.png"
path_verao     <- "iconos/verao.png"
path_outono    <- "iconos/outono.png"

path_icono_alt <- "iconos/altitude_new_icon.png" 
path_icono_mar <- "iconos/mar_icon.png"       # Ajusta a tu ruta real
tamanho_icono_perfiles <- 40                  # Tamaño en píxeles del icono

#tamanho_icono  <- 35 
tamanho_icono  <- 50 

# -------------------- TEMA VISUAL (SEM LEYENDAS) --------------------
tema_minimal <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA), 
    plot.background  = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(), 
    #plot.title = element_text(face = "bold", family="Poppins", size = 35, hjust = 0.5),
    plot.title = element_blank(),
    legend.position = "none"
  )

# -------------------- MESES EM PORTUGUÊS --------------------
meses_pt <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

# Cores dos "quesitos" internos e sincronização com as estações externas
cores_dos_meses <- c(
  "#DCEEFF", "#DCEEFF",             # Jan, Fev (Inverno)
  "#E2F5D3", "#E2F5D3", "#E2F5D3", # Mar, Abr, Mai (Primavera)
  "#FFEAD2", "#FFEAD2", "#FFEAD2", # Jun, Jul, Ago (Verão)
  "#F5E1ED", "#F5E1ED", "#F5E1ED", # Set, Out, Nov (Outono)
  "#DCEEFF"                         # Dez (Inverno)
)

# Paleta de cores para as Estações (Anel Exterior)
cores_estacoes <- c(
  "Inverno"   = "#DCEEFF", 
  "Primavera" = "#E2F5D3", 
  "Verão"     = "#FFEAD2", 
  "Outono"    = "#F5E1ED",
  "Inverno "  = "#DCEEFF"
)

# Cor cinza homogéneo para o anel intermédio dos meses
#cor_anel_meses_cinza <- "#F4F4F4"
cor_anel_meses_cinza <- NA

# -------------------- IMPORTAR DADOS --------------------
message("> A processar dados...")
arquivo_csv <- "./occur_data/observations-747569.csv/observations-747569.csv"

galhas_inat <- read.csv(arquivo_csv) %>%
  mutate(
    latitude = ifelse(!is.na(private_latitude) & private_latitude != "", private_latitude, latitude),
    longitude = ifelse(!is.na(private_longitude) & private_longitude != "", private_longitude, longitude)
  ) %>%
  filter(!is.na(latitude), !is.na(longitude))

# -------------------- OBJECTO ESPACIAL (ALINHAMENTO DE CRS IMEDIATO) --------------------
target_crs <- 3763 

galhas_sf <- st_as_sf(galhas_inat, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(target_crs)

galhas_sf_4326 <- st_transform(galhas_sf, 4326)

# -------------------- ALTITUDE --------------------
message("> A calcular altitude...")
elev_data <- get_elev_point(galhas_sf_4326, prj = st_crs(4326)$proj4string, src = "aws")
galhas_inat$altitude <- elev_data$elevation

# -------------------- DISTÂNCIA AO MAR --------------------
message("> A calcular distância ao mar...")
coast <- ne_coastline(scale = "medium", returnclass = "sf") %>% st_transform(4326)
dist_matrix <- st_distance(galhas_sf_4326, coast)
galhas_inat$distance_to_sea <- as.numeric(apply(dist_matrix, 1, min)) / 1000

# Exportar base limpa
write.csv2(galhas_inat, file.path(base_dir,"Galhas_inat_altitud_distancia.csv"), row.names = FALSE)

species_list <- sort(unique(galhas_inat$scientific_name))

# ======================================================
# FENOLOGIA GLOBAL
# ======================================================
message("> A gerar fenologia global...")

fenologia_global <- galhas_inat %>%
  filter(!is.na(observed_on)) %>%
  mutate(
    data = ymd(observed_on),
    mes_num = month(data),
    mes = factor(meses_pt[mes_num], levels = meses_pt)
  ) %>%
  count(mes, mes_num) %>%
  complete(mes = factor(meses_pt, levels = meses_pt), fill = list(n = 0)) %>%
  mutate(mes_num = match(mes, meses_pt)) %>%
  arrange(mes_num)

# -------------------- POLAR PLOT GLOBAL --------------------
max_n_global <- max(fenologia_global$n, na.rm = TRUE)
if(max_n_global == 0) max_n_global <- 1

r_max_global        <- max_n_global * 1.05  
r_arco_mes_min_g    <- max_n_global * 1.14  
r_arco_mes_max_g    <- max_n_global * 1.30  # Expandido ligeiramente para abrigar fonte 10
r_meses_global      <- max_n_global * 1.22  # Ajustado para centrar a fonte tamanho 10

r_arco_est_min_g    <- max_n_global * 1.36  
r_arco_est_max_g    <- max_n_global * 1.54  
r_estaciones_global <- max_n_global * 1.45  

# Quesitos internos de fundo
quesitos_fundo_global <- data.frame(
  xmin = (1:12) - 0.5,
  xmax = (1:12) + 0.5,
  cor  = cores_dos_meses
)

estaciones_global <- data.frame(
  estacao = factor(c("Inverno", "Primavera", "Verão", "Outono", "Inverno "), 
                   levels = c("Inverno", "Primavera", "Verão", "Outono", "Inverno ")),
  xmin = c(11.5, 2.5,  5.5,  8.5,  0.5),
  xmax = c(12.5, 5.5,  8.5, 11.5,  2.5)
)

arcos_meses_global <- data.frame(xmin = (1:12) - 0.5, xmax = (1:12) + 0.5)
divisorias_global <- data.frame(x = c(2.5, 5.5, 8.5, 11.5))

labels_estaciones_global <- data.frame(
  x = c(1, 4, 7, 10), 
  y = rep(r_estaciones_global, 4),
  label = c(
    paste0("<img src='", path_inverno, "' width='", tamanho_icono, "'/>"),
    paste0("<img src='", path_primavera, "' width='", tamanho_icono, "'/>"),
    paste0("<img src='", path_verao, "' width='", tamanho_icono, "'/>"),
    paste0("<img src='", path_outono, "' width='", tamanho_icono, "'/>")
  )
)

labels_meses_global <- data.frame(x = 1:12, y = rep(r_meses_global, 12), label = meses_pt)

g_feno_global_polar <- ggplot() +
  # 1. Quesitos coloridos no interior (Fundo dos dados)
  geom_rect(data = quesitos_fundo_global, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = r_max_global, fill = cor),
            alpha = 0.5, color = NA, inherit.aes = FALSE) +
  scale_fill_identity() + 
  
  # 2. Barras centrais de dados (Gradiente verde)
  ggnewscale::new_scale_fill() +
  geom_col(data = fenologia_global, aes(x = mes_num, y = n, fill = n), width = 0.85, color = "white", linewidth = 0.4) +
  scale_fill_gradient(low = "#e0ecd1", high = "#2d6a4f") +
  
  # 3. Arcos dos meses em cinzento homogéneo
  geom_rect(data = arcos_meses_global,
            aes(xmin = xmin, xmax = xmax, ymin = r_arco_mes_min_g, ymax = r_arco_mes_max_g),
            fill = cor_anel_meses_cinza, color = "white", linewidth = 0.4, inherit.aes = FALSE) +
  
  # 4. Arcos exteriores das estações sem bordas automáticas (Evita corte no Inverno)
  ggnewscale::new_scale_fill() +
  geom_rect(data = estaciones_global, 
            aes(xmin = xmin, xmax = xmax, ymin = r_arco_est_min_g, ymax = r_arco_est_max_g, fill = estacao), 
            alpha = 0.9, color = NA, inherit.aes = FALSE) +
  scale_fill_manual(values = cores_estacoes) +
  
  # 5. Linhas divisórias manuais das estações exteriores
  geom_segment(data = divisorias_global, aes(x = x, xend = x, y = r_arco_est_min_g, yend = r_arco_est_max_g), 
               color = "white", linewidth = 0.6, inherit.aes = FALSE) +
  
  # Textos dos Meses (Aumentado para tamanho de fonte 10)
  geom_text(data = labels_meses_global, aes(x = x, y = y, label = label), family = "Poppins", fontface = "bold", size = 10, color = "black", inherit.aes = FALSE) +
  
  # Ícones das Estações
  ggtext::geom_richtext(data = labels_estaciones_global, aes(x = x, y = y, label = label), 
                        fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"), inherit.aes = FALSE) +
  
  coord_polar(start = 0) +
  scale_x_continuous(limits = c(0.5, 12.5), breaks = 1:12) +
  ylim(0, max_n_global * 1.66) + 
  labs(title = "Fenologia Global Circular") +
  tema_minimal +
  theme(panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background  = element_rect(fill = "transparent", color = NA))

ggsave(file.path(dir_feno,"Fenologia_global_circular.png"), g_feno_global_polar, width = 12, height = 10, dpi = 300)


# ======================================================
# LOOP POR ESPÉCIE
# ======================================================
message("> A gerar gráficos por espécie...")

for(sp in species_list) {
  sp_data <- galhas_inat %>% filter(scientific_name == sp)
  if(nrow(sp_data) < 2) next
  nome_seguro <- str_replace_all(sp, "[^A-Za-z0-9]", "_")
  
  fecha_hoy <- format(Sys.Date(), "%d-%m-%Y")
  texto_caption <- paste0("Dados: iNaturalist (", fecha_hoy, ") | Portugal Continental e Ilhas")

  
  # Alineamos el icono exactamente sobre el nivel de la nueva línea flotante (y = -0.15)
  data_icono_alt <- data.frame(x = -Inf, y = -0.15, label = paste0("<img src='", path_icono_alt, "' width='", tamanho_icono_perfiles, "'/>"))
  data_icono_mar <- data.frame(x = -Inf, y = -0.15, label = paste0("<img src='", path_icono_mar, "' width='", tamanho_icono_perfiles, "'/>"))
  
  puedo_calcular_alt <- nrow(sp_data) > 1 && var(sp_data$altitude, na.rm = TRUE) > 0
  puedo_calcular_mar <- nrow(sp_data) > 1 && var(sp_data$distance_to_sea, na.rm = TRUE) > 0
  
  # ==============================================================================
  # 1. ALTITUDE (EIXO FLUTUANTE PERSONALIZADO)
  # ==============================================================================
  
  g1_base <- ggplot() + 
    geom_jitter(data = sp_data, aes(x = altitude, y = 0), width = 0, height = 0.05, color = "#674635", alpha = 0.5, size = 3)
  
  texto_rango_alt <- ""
  if (puedo_calcular_alt) {
    stats_alt <- sp_data %>% 
      summarise(
        min_v = min(altitude, na.rm = TRUE),
        max_v = max(altitude, na.rm = TRUE),
        q25   = quantile(altitude, 0.25, na.rm = TRUE),
        q75   = quantile(altitude, 0.75, na.rm = TRUE),
        med   = median(altitude, na.rm = TRUE)
      )
    
    g1_base <- g1_base +
      geom_segment(data = stats_alt, aes(x = min_v, xend = max_v, y = 0, yend = 0), color = "#674635", linewidth = 3, alpha = 0.3) +
      geom_segment(data = stats_alt, aes(x = q25, xend = q75, y = 0, yend = 0), color = "#674635", linewidth = 8, alpha = 0.7) +
      geom_point(data = stats_alt, aes(x = med, y = 0), color = "black", size = 5)
  }
  
  breaks_alt <- ggplot_build(g1_base)$layout$panel_params[[1]]$x$breaks
  breaks_alt <- breaks_alt[!is.na(breaks_alt)]
  df_labels_alt <- data.frame(x = breaks_alt, y = -0.17, label = as.character(breaks_alt))
  
  min_real_alt <- min(sp_data$altitude, na.rm = TRUE)
  max_real_alt <- max(sp_data$altitude, na.rm = TRUE)
  
  g1_base <- g1_base + 
    geom_segment(aes(x = min_real_alt, xend = max_real_alt, y = -0.12, yend = -0.12), color = "grey60", linewidth = 0.5)
  
  x_leyenda_alt <- max_real_alt
  
  g1 <- g1_base +
    geom_text(data = df_labels_alt, aes(x = x, y = y, label = label), family = "Poppins", size = 15, color = "black") +
    
    # -----------------------------------------------------------------
  # LEGENDA GRÁFICA DIVULGATIVA (ALTITUDE)
  annotate("segment", x = x_leyenda_alt * 1.10, xend = x_leyenda_alt * 1.20, y = 0.04, yend = 0.04, color = "#674635", linewidth = 3, alpha = 0.3) +
    annotate("text", x = x_leyenda_alt * 1.22, y = 0.04, label = "Intervalo total", family = "Poppins", size = 12, color = "grey30", vjust = 0.5, hjust = 0) +
    
    annotate("segment", x = x_leyenda_alt * 1.10, xend = x_leyenda_alt * 1.20, y = 0, yend = 0, color = "#674635", linewidth = 8, alpha = 0.7) +
    annotate("text", x = x_leyenda_alt * 1.22, y = 0, label = "Intervalo mais frequente", family = "Poppins", size = 12, color = "grey30", vjust = 0.5, hjust = 0) +
    
    annotate("point", x = x_leyenda_alt * 1.15, y = -0.04, color = "black", size = 5) +
    annotate("text", x = x_leyenda_alt * 1.22, y = -0.04, label = "Altitude mais frequente", family = "Poppins", size = 12, color = "grey30", vjust = 0.5, hjust = 0) +
    # -----------------------------------------------------------------
  
  labs(x = "Altitude (m)", y = NULL) + 
    theme_minimal() + 
    scale_y_continuous(limits = c(-0.25, 0.25), expand = c(0, 0)) + 
    theme(
      panel.background = element_rect(fill = "transparent", color = NA), 
      plot.background  = element_rect(fill = "transparent", color = NA),
      axis.line.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.title.x = element_text(face = "bold", family = "Poppins", size = 35, hjust = 0.5, margin = margin(t = -15)),
      plot.caption = element_text(hjust = 0.5, size = 25, family = "Poppins", color = "grey30", margin = margin(t = 10))
    ) +
    # MODIFICACIÓN: y = y + 0.08 desplaza el icono hacia arriba en el eje vertical
    ggtext::geom_richtext(data = data_icono_alt, aes(x = x, y = y + 0.08, label = label), fill = NA, label.color = NA, label.padding = grid::unit(c(10, 10, 10, 10), "pt"), vjust = 0.5, hjust = 1, inherit.aes = FALSE) +
    
    coord_cartesian(xlim = c(min_real_alt, max_real_alt * 1.05), clip = "off") +
    theme(plot.margin = margin(t = 5, r = 200, b = 35, l = 60))
  
  ggsave(file.path(dir_sp, paste0(nome_seguro,"_altitude_linear.png")), g1, width = 10, height = 3.8, dpi = 300)
  
  
  # ==============================================================================
  # 2. DISTÂNCIA AO MAR (EIXO FLUTUANTE PERSONALIZADO)
  # ==============================================================================
  
  g2_base <- ggplot() + 
    geom_jitter(data = sp_data, aes(x = distance_to_sea, y = 0), width = 0, height = 0.05, color = "#841A1A", alpha = 0.5, size = 3)
  
  texto_rango_mar <- ""
  if (puedo_calcular_mar) {
    stats_mar <- sp_data %>% 
      summarise(
        min_v = min(distance_to_sea, na.rm = TRUE),
        max_v = max(distance_to_sea, na.rm = TRUE),
        q25   = quantile(distance_to_sea, 0.25, na.rm = TRUE),
        q75   = quantile(distance_to_sea, 0.75, na.rm = TRUE),
        med   = median(distance_to_sea, na.rm = TRUE)
      )
    
    g2_base <- g2_base +
      geom_segment(data = stats_mar, aes(x = min_v, xend = max_v, y = 0, yend = 0), color = "#841A1A", linewidth = 3, alpha = 0.3) +
      geom_segment(data = stats_mar, aes(x = q25, xend = q75, y = 0, yend = 0), color = "#841A1A", linewidth = 8, alpha = 0.7) +
      geom_point(data = stats_mar, aes(x = med, y = 0), color = "black", size = 5)
  }
  
  g2_base <- g2_base + scale_x_continuous(expand = c(0, 0.05))
  
  breaks_mar <- ggplot_build(g2_base)$layout$panel_params[[1]]$x$breaks
  breaks_mar <- breaks_mar[!is.na(breaks_mar)]
  df_labels_mar <- data.frame(x = breaks_mar, y = -0.17, label = as.character(breaks_mar))
  
  g2_base <- g2_base + 
    geom_segment(aes(x = 0, 
                     xend = max(sp_data$distance_to_sea, na.rm = TRUE), 
                     y = -0.12, yend = -0.12), color = "grey60", linewidth = 0.5)
  
  x_leyenda_mar <- max(sp_data$distance_to_sea, na.rm = TRUE)
  
  g2 <- g2_base +
    geom_text(data = df_labels_mar, aes(x = x, y = y, label = label), family = "Poppins", size = 15, color = "black") +
    
    # -----------------------------------------------------------------
  # LEGENDA GRÁFICA DIVULGATIVA (DISTÂNCIA AO MAR)
  annotate("segment", x = x_leyenda_mar * 1.05, xend = x_leyenda_mar * 1.15, y = 0.04, yend = 0.04, color = "#841A1A", linewidth = 3, alpha = 0.3) +
    annotate("text", x = x_leyenda_mar * 1.17, y = 0.04, label = "Intervalo total", family = "Poppins", size = 12, color = "grey30", vjust = 0.5, hjust = 0) +
    
    annotate("segment", x = x_leyenda_mar * 1.05, xend = x_leyenda_mar * 1.15, y = 0, yend = 0, color = "#841A1A", linewidth = 8, alpha = 0.7) +
    annotate("text", x = x_leyenda_mar * 1.17, y = 0, label = "Intervalo mais frequente", family = "Poppins", size = 12, color = "grey30", vjust = 0.5, hjust = 0) +
    
    annotate("point", x = x_leyenda_mar * 1.10, y = -0.04, color = "black", size = 5) +
    annotate("text", x = x_leyenda_mar * 1.17, y = -0.04, label = "Distância mais frequente", family = "Poppins", size = 12, color = "grey30", vjust = 0.5, hjust = 0) +
    # -----------------------------------------------------------------
  
  labs(x = "Distância ao mar (km)", y = NULL) + 
    theme_minimal() + 
    scale_y_continuous(limits = c(-0.25, 0.25), expand = c(0, 0)) + 
    theme(
      panel.background = element_rect(fill = "transparent", color = NA), 
      plot.background  = element_rect(fill = "transparent", color = NA),
      axis.line.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.title.x = element_text(face = "bold", family = "Poppins", size = 35, hjust = 0.5, margin = margin(t = -15)),
      plot.caption = element_text(hjust = 0.5, size = 25, family = "Poppins", color = "grey30", margin = margin(t = 10))
    ) + 
    # MODIFICACIÓN: y = y + 0.08 desplaza el icono hacia arriba en el eje vertical
    ggtext::geom_richtext(data = data_icono_mar, aes(x = x, y = y + 0.08, label = label), fill = NA, label.color = NA, label.padding = grid::unit(c(10, 10, 10, 10), "pt"), vjust = 0.5, hjust = 1, inherit.aes = FALSE) +
    
    coord_cartesian(xlim = c(0, max(sp_data$distance_to_sea, na.rm = TRUE) * 1.05), clip = "off") +
    theme(plot.margin = margin(t = 5, r = 200, b = 45, l = 60))
  
  ggsave(file.path(dir_sp, paste0(nome_seguro,"_distancia_mar_linear.png")), g2, width = 10, height = 3.8, dpi = 300)
  
  
  # ==============================================================================
  # 3. ALTITUDE VS DISTÂNCIA AO MAR
  # ==============================================================================
  
  g3 <- ggplot(sp_data, aes(x = distance_to_sea, y = altitude)) + 
    geom_point(color = "purple", alpha = 0.5) +
    geom_smooth(method = "loess", color = "red", formula = y ~ x) + 
    labs(x = "Distância ao mar (km)", y = "Altitude (m)", caption = texto_caption) + 
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA), 
      plot.background  = element_rect(fill = "transparent", color = NA),
      axis.line = element_line(color = "grey", linewidth = 0.5),
      axis.text = element_text(family = "Poppins", size = 35),
      axis.title = element_text(face = "bold", family = "Poppins", size = 40),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_blank(),
      legend.position = "none",
      plot.caption = element_text(hjust = 0.5, size = 25, family = "Poppins")
    )   
  
  ggsave(file.path(dir_sp, paste0(sp, "_altitude_vs_distancia_mar.png")), g3, width = 10, height = 6)
  
  
  if("observed_on" %in% names(sp_data)) {
    dados_feno <- sp_data %>%
      filter(!is.na(observed_on)) %>%
      mutate(data = ymd(observed_on), mes_num = month(data), mes = factor(meses_pt[mes_num], levels = meses_pt)) %>%
      count(scientific_name, mes, mes_num) %>%
      complete(mes = factor(meses_pt, levels = meses_pt), fill = list(n = 0)) %>%
      mutate(mes_num = match(mes, meses_pt)) %>% arrange(mes_num)
    
    max_n_sp <- max(dados_feno$n, na.rm = TRUE)
    if(max_n_sp == 0) max_n_sp <- 1
    
    r_max_sp           <- max_n_sp * 1.05
    r_arco_mes_min_sp  <- max_n_sp * 1.14
    r_arco_mes_max_sp  <- max_n_sp * 1.30  # Expandido ligeiramente para abrigar fonte 10
    r_meses_sp         <- max_n_sp * 1.22  # Ajustado para centrar a fonte tamanho 10
    r_arco_est_min_sp  <- max_n_sp * 1.36  
    r_arco_est_max_sp  <- max_n_sp * 1.54  
    r_estaciones_sp    <- max_n_sp * 1.45  
    
    quesitos_fundo_sp <- data.frame(xmin = (1:12) - 0.5, xmax = (1:12) + 0.5, cor = cores_dos_meses)
    
    estaciones_sp <- data.frame(
      estacao = factor(c("Inverno", "Primavera", "Verão", "Outono", "Inverno "), levels = c("Inverno", "Primavera", "Verão", "Outono", "Inverno ")),
      xmin = c(11.5, 2.5,  5.5,  8.5,  0.5), xmax = c(12.5, 5.5,  8.5, 11.5,  2.5)
    )
    arcos_meses_sp <- data.frame(xmin = (1:12) - 0.5, xmax = (1:12) + 0.5)
    divisorias_sp  <- data.frame(x = c(2.5, 5.5, 8.5, 11.5))
    
    labels_estaciones_sp <- data.frame(
      x = c(1, 4, 7, 10), y = rep(r_estaciones_sp, 4),
      label = c(
        paste0("<img src='", path_inverno, "' width='", tamanho_icono - 5, "'/>"),
        paste0("<img src='", path_primavera, "' width='", tamanho_icono - 5, "'/>"),
        paste0("<img src='", path_verao, "' width='", tamanho_icono - 5, "'/>"),
        paste0("<img src='", path_outono, "' width='", tamanho_icono - 5, "'/>")
      )
    )
    labels_meses_sp      <- data.frame(x = 1:12, y = rep(r_meses_sp, 12), label = meses_pt)
    
    gcirc <- ggplot() +
      # 1. Quesitos internos de fundo
      geom_rect(data = quesitos_fundo_sp, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = r_max_sp, fill = cor),
                alpha = 0.5, color = NA, inherit.aes = FALSE) +
      scale_fill_identity() +
      
      # 2. Barras centrais
      ggnewscale::new_scale_fill() +
      geom_col(data = dados_feno, aes(x = mes_num, y = n, fill = "Observável"), width = 0.85, color = "white", linewidth = 0.4) +
      #scale_fill_gradient(low = "#e0ecd1", high = "#2d6a4f") +
      scale_fill_manual(name = NULL, values = c("Observável" = "#2d6a4f")) +
      # 3. Arcos dos meses cinzentos
      geom_rect(data = arcos_meses_sp, aes(xmin = xmin, xmax = xmax, ymin = r_arco_mes_min_sp, ymax = r_arco_mes_max_sp), fill = cor_anel_meses_cinza, color = "white", linewidth = 0.4, inherit.aes = FALSE) +
      
      # 4. Arcos exteriores das estações sem bordas automáticas
      ggnewscale::new_scale_fill() +
      geom_rect(data = estaciones_sp, aes(xmin = xmin, xmax = xmax, ymin = r_arco_est_min_sp, ymax = r_arco_est_max_sp, fill = estacao), alpha = 0.9, color = NA, inherit.aes = FALSE) +
      scale_fill_manual(name = "Estações", values = cores_estacoes, breaks = c("Inverno", "Primavera", "Verão", "Outono")) +
      
      # 5. Linhas divisórias manuais
      geom_segment(data = divisorias_sp, aes(x = x, xend = x, y = r_arco_est_min_sp, yend = r_arco_est_max_sp), color = "white", linewidth = 0.5, inherit.aes = FALSE) +
      
      # Textos dos Meses (Aumentado para tamanho de fonte 10)
      geom_text(data = labels_meses_sp, aes(x = x, y = y, label = label), family = "Poppins", fontface = "bold", size = 20, color = "black", inherit.aes = FALSE) +
      
      ggtext::geom_richtext(data = labels_estaciones_sp, aes(x = x, y = y, label = label), fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"), inherit.aes = FALSE) +
      
      coord_polar(start = 0) +
      scale_x_continuous(limits = c(0.5, 12.5), breaks = 1:12) +
      ylim(0, max_n_sp * 1.66) +
      labs(title = bquote(paste(italic(.(sp)))), caption = texto_caption, family = "Poppins") +
      tema_minimal +
      theme(panel.background = element_rect(fill = "transparent", color = NA), 
            plot.background  = element_rect(fill = "transparent", color = NA),
            legend.position = "right",
            legend.title = element_text(size = 50, face = "bold", family = "Poppins"), # Tamaño del título
            legend.text = element_text(size = 40, family = "Poppins"),
            plot.caption = element_text(hjust = 0.5, size = 25, family = "Poppins"))
    
    ggsave(file.path(dir_feno, paste0(nome_seguro, "_fenologia_circular.png")), gcirc, width = 10, height = 9, dpi = 300)
  }
}







# ======================================================
# SEÇÃO RIQUEZA DE ESPÉCIES CIRCULAR
# ======================================================
richness_df <- galhas_inat %>%
  group_by(latitude, longitude) %>%
  summarise(richness = n_distinct(scientific_name), altitude = mean(altitude), distance_to_sea = mean(distance_to_sea), .groups = "drop")

fenologia_riqueza_mes <- galhas_inat %>%
  filter(!is.na(observed_on), !is.na(scientific_name)) %>%
  mutate(data = ymd(observed_on), mes_num = month(data), mes = factor(meses_pt[mes_num], levels = meses_pt)) %>%
  group_by(mes, mes_num) %>%
  summarise(riqueza = n_distinct(scientific_name), .groups = "drop") %>%
  arrange(mes_num) %>%
  complete(mes = factor(meses_pt, levels = meses_pt), fill = list(riqueza = 0)) %>%
  mutate(mes_num = match(mes, meses_pt))

max_rich_mes <- max(fenologia_riqueza_mes$riqueza, na.rm = TRUE)
if(max_rich_mes == 0) max_rich_mes <- 1

r_max_rich         <- max_rich_mes * 1.05
r_arco_mes_min_r   <- max_rich_mes * 1.14
r_arco_mes_max_r   <- max_rich_mes * 1.30  # Expandido ligeiramente para abrigar fonte 10
r_meses_rich       <- max_rich_mes * 1.22  # Ajustado para centrar a fonte tamanho 10
r_arco_est_min_r   <- max_rich_mes * 1.36  
r_arco_est_max_r   <- max_rich_mes * 1.54  
r_estaciones_rich  <- max_rich_mes * 1.45  

quesitos_fundo_rich <- data.frame(xmin = (1:12) - 0.5, xmax = (1:12) + 0.5, cor = cores_dos_meses)

estaciones_rich <- data.frame(
  estacao = factor(c("Inverno", "Primavera", "Verão", "Outono", "Inverno "), levels = c("Inverno", "Primavera", "Verão", "Outono", "Inverno ")),
  xmin = c(11.5, 2.5,  5.5,  8.5,  0.5), xmax = c(12.5, 5.5,  8.5, 11.5,  2.5)
)
arcos_meses_rich       <- data.frame(xmin = (1:12) - 0.5, xmax = (1:12) + 0.5)
divisorias_rich        <- data.frame(x = c(2.5, 5.5, 8.5, 11.5))

labels_estaciones_rich <- data.frame(
  x = c(1, 4, 7, 10), y = rep(r_estaciones_rich, 4),
  label = c(
    paste0("<img src='", path_inverno, "' width='", tamanho_icono, "'/>"),
    paste0("<img src='", path_primavera, "' width='", tamanho_icono, "'/>"),
    paste0("<img src='", path_verao, "' width='", tamanho_icono, "'/>"),
    paste0("<img src='", path_outono, "' width='", tamanho_icono, "'/>")
  )
)
labels_meses_rich      <- data.frame(x = 1:12, y = rep(r_meses_rich, 12), label = meses_pt)

g_rich_mes_polar <- ggplot() +
  # 1. Quesitos internos de fundo
  geom_rect(data = quesitos_fundo_rich, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = r_max_rich, fill = cor),
            alpha = 0.5, color = NA, inherit.aes = FALSE) +
  scale_fill_identity() +
  
  # 2. Barras centrais
  ggnewscale::new_scale_fill() +
  geom_col(data = fenologia_riqueza_mes, aes(x = mes_num, y = riqueza, fill = riqueza), width = 0.85, color = "white", linewidth = 0.4) +
  scale_fill_gradient(low = "#e0ecd1", high = "#2d6a4f") +
  
  # 3. Arcos dos meses cinzentos
  geom_rect(data = arcos_meses_rich, aes(xmin = xmin, xmax = xmax, ymin = r_arco_mes_min_r, ymax = r_arco_mes_max_r), fill = cor_anel_meses_cinza, color = "white", linewidth = 0.4, inherit.aes = FALSE) +
  
  # 4. Arcos exteriores das estações sem bordas automáticas
  ggnewscale::new_scale_fill() +
  geom_rect(data = estaciones_rich, aes(xmin = xmin, xmax = xmax, ymin = r_arco_est_min_r, ymax = r_arco_est_max_r, fill = estacao), alpha = 0.9, color = NA, inherit.aes = FALSE) +
  scale_fill_manual(values = cores_estacoes) +
  
  # 5. Linhas divisórias manuais
  geom_segment(data = divisorias_rich, aes(x = x, xend = x, y = r_arco_est_min_r, yend = r_arco_est_max_r), color = "white", linewidth = 0.5, inherit.aes = FALSE) +
  
  # Textos dos Meses (Aumentado para tamanho de fonte 10)
  geom_text(data = labels_meses_rich, aes(x = x, y = y, label = label), family = "Poppins", fontface = "bold", size = 10, color = "black", inherit.aes = FALSE) +
  
  ggtext::geom_richtext(data = labels_estaciones_rich, aes(x = x, y = y, label = label), fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"), inherit.aes = FALSE) +
  
  coord_polar(start = 0) +
  scale_x_continuous(limits = c(0.5, 12.5), breaks = 1:12) +
  ylim(0, max_rich_mes * 1.66) +
  labs(title = "Riqueza de espécies (circular)") +
  tema_minimal +
  theme(panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background  = element_rect(fill = "transparent", color = NA))

ggsave(file.path(dir_rich, "Riqueza_especies_por_mes_circular.png"), g_rich_mes_polar, width = 12, height = 10, dpi = 300)

cat("\n✔ Execução completa finalizada com fonte de tamanho 10 para os meses.\n")











# ======================================================
# RANGO ALTITUDINAL POR ESPECIE (ORDENADO)
# ======================================================

altitude_species <- galhas_inat %>%
  filter(
    !is.na(scientific_name),
    !is.na(altitude)) %>%
  group_by(scientific_name) %>%
  summarise(
    alt_min  = min(altitude, na.rm = TRUE),
    alt_max  = max(altitude, na.rm = TRUE),
    alt_mean = mean(altitude, na.rm = TRUE),
    range_alt = alt_max - alt_min,
    n = n(), .groups = "drop") %>%
  arrange(range_alt) %>%
  mutate(scientific_name = factor(scientific_name, levels = scientific_name)) %>%
  mutate(sp_short = gsub("(\\w+)\\s(\\w+).*", "\\1 \\2", scientific_name))

write.csv2(altitude_species, file.path(base_dir,"altitude_species_range.csv"),row.names = FALSE)

g_alt_range <- ggplot(altitude_species) +
  # rango min-max
  geom_segment(aes(x = scientific_name, xend = scientific_name, y = alt_min, yend = alt_max), color = "grey60", linewidth = 0.8) +
  # media
  geom_point(aes(x = scientific_name, y = alt_mean), color = "#674635", size = 0.5) +
  labs(title = "Rango altitudinal por espécie (ordenado por amplitude)", x = "Espécie",y = "Altitude (m)") +
  tema_minimal +
  theme(panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background  = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(family="Poppins", angle = 45,hjust = 1,size = 5),
        axis.text.y =element_text(family="Poppins", size = 20),
        axis.line = element_line(color = "grey", linewidth = 0.5),
        axis.title = element_text(face = "bold", family="Poppins", size = 25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        legend.position = "none"
  )
ggsave(file.path(dir_rich, "Rango_altitudinal_por_especie_ordenado.png"),
       g_alt_range, width = 12, height = 7, dpi = 300)

# ======================================================
# RANGO DE DISTANCIA AL MAR POR ESPECIE
# ======================================================

distance_species <- galhas_inat %>%
  filter(
    !is.na(scientific_name),
    !is.na(distance_to_sea)) %>%
  group_by(scientific_name) %>%
  summarise(
    dist_min  = min(distance_to_sea, na.rm = TRUE),
    dist_max  = max(distance_to_sea, na.rm = TRUE),
    dist_mean = mean(distance_to_sea, na.rm = TRUE),
    range_dist = dist_max - dist_min,
    n = n(),  .groups = "drop") %>%
  arrange(range_dist) %>%
  mutate(scientific_name = factor(scientific_name, levels = scientific_name))

write.csv2(distance_species, file.path(base_dir,"sea_distance_species_range.csv"),row.names = FALSE)

g_dist_range <- ggplot(distance_species) +
  # rango min–max
  geom_segment(aes(x = scientific_name, xend = scientific_name, y = dist_min, yend = dist_max), color = "grey60", linewidth = 0.8 ) +
  # media
  geom_point(aes(x = scientific_name, y = dist_mean), color = "#841A1A", size = 0.5) +
  labs( title = "Rango de distância ao mar por espécie", x = "Espécie", y = "Distância ao mar (km)" ) +
  tema_minimal +
  theme(panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background  = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(family="Poppins", angle = 45,hjust = 1,size = 5),
        axis.text.y =element_text(family="Poppins", size = 20),
        axis.line = element_line(color = "grey", linewidth = 0.5),
        axis.title = element_text(face = "bold", family="Poppins", size = 25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        legend.position = "none"
  )

ggsave(
  file.path(dir_rich, "Rango_distancia_mar_por_especie_ordenado.png"),
  g_dist_range, width = 12, height = 7,  dpi = 300)

# ==========================================================
# SECCIÓN 2: TODOS LOS GRÁFICOS DEL SCRIPT 2 (Riqueza)
# ==========================================================
richness_df <- galhas_inat %>%
  group_by(latitude, longitude) %>%
  summarise(richness = n_distinct(scientific_name), altitude = mean(altitude), distance_to_sea = mean(distance_to_sea), .groups = "drop")

cor_alt <- cor.test(richness_df$richness, richness_df$altitude, method="spearman")
cor_dist <- cor.test(richness_df$richness, richness_df$distance_to_sea, method="spearman")

# A. Densidad Riqueza Altitud
gr1 <- ggplot(richness_df, aes(x=altitude)) + geom_density(fill="#674635", alpha=0.4, color="#674635") +
  #labs(title="Riqueza: Distribuição Altitudinal", x="Altitude (m)", y="Densidade") +
  labs(x="Altitude (m)", y="Densidade") +
  #tema_minimal+
  theme(panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background  = element_rect(fill = "transparent", color = NA),
        axis.text =element_text(family="Poppins", size = 20),
        axis.line = element_line(color = "grey", linewidth = 0.5),
        axis.title = element_text(face = "bold", family="Poppins", size = 25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank()
  )
ggsave(file.path(dir_rich, "Altitude_richness.png"), gr1, width=10, height=6, dpi = 300)

# B. Densidad Riqueza Distancia
gr2 <- ggplot(richness_df, aes(x=distance_to_sea)) + geom_density(fill="#841A1A", alpha=0.4, color="#841A1A") +
  labs(title="Riqueza: Distância ao Mar", x="Distância (km)", y="Densidade") + 
  #tema_minimal+
  theme(panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background  = element_rect(fill = "transparent", color = NA),
        axis.text =element_text(family="Poppins", size = 20),
        axis.line = element_line(color = "grey", linewidth = 0.5),
        axis.title = element_text(face = "bold", family="Poppins", size = 25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank()
  )
ggsave(file.path(dir_rich, "Distancia_mar_richness.png"), gr2, width=10, height=6, dpi = 300)

# C. Bivariado Riqueza (Plasma + Correlación)
gr3 <- ggplot(richness_df, aes(x=distance_to_sea, y=altitude, color=richness)) +
  geom_point(alpha=0.6, size=2.5) + scale_color_viridis_c(option="plasma") +
  geom_smooth(method="loess", color="red", formula=y~x) +
  annotate("label", x=Inf, y=Inf, hjust=1.1, vjust=1.5, label=paste0("Rho: ", round(cor_alt$estimate, 2), "\np < 0.01")) +
  labs(title="Relación Riqueza - Espaço", x="Distância (km)", y="Altitud (m)", color="Riqueza") + 
  #tema_minimal+
  theme(panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background  = element_rect(fill = "transparent", color = NA),
        axis.text =element_text(family="Poppins", size = 20),
        axis.line = element_line(color = "grey", linewidth = 0.5),
        axis.title = element_text(face = "bold", family="Poppins", size = 25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank()
  )
ggsave(file.path(dir_rich, "Altitude_vs_distancia_richness.png"), gr3, width=10, height=6, dpi = 300)


# ======================================================
# RIQUEZA DE ESPECIES POR MES
# ======================================================

fenologia_riqueza_mes <- galhas_inat %>%
  filter(!is.na(observed_on), !is.na(scientific_name)) %>%
  mutate(
    data = ymd(observed_on),
    mes_num = month(data),
    mes = factor(meses_pt[mes_num], levels = meses_pt)
  ) %>%
  group_by(mes, mes_num) %>%
  summarise(
    riqueza = n_distinct(scientific_name),
    .groups = "drop"
  ) %>%
  arrange(mes_num) %>%
  complete(
    mes = factor(meses_pt, levels = meses_pt),
    fill = list(riqueza = 0)
  ) %>%
  mutate(mes_num = match(mes, meses_pt))

# -------------------- BARPLOT --------------------

g_rich_mes <- ggplot(
  fenologia_riqueza_mes,
  aes(x = mes, y = riqueza)
) +
  geom_col(fill = "#2E86AB", width = 0.7) +
  geom_line(aes(group = 1), color = "#1B4965", linewidth = 0.9) +
  geom_point(color = "#1B4965", size = 2.5) +
  scale_x_discrete(drop = FALSE) +
  labs(
    title = "Riqueza de espécies por mês",
    x = "Mês",
    y = "Riqueza de espécies"
  ) +
  #tema_minimal +
  theme(panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background  = element_rect(fill = "transparent", color = NA),
        axis.text =element_text(family="Poppins", size = 20),
        axis.line = element_line(color = "grey", linewidth = 0.5),
        axis.title = element_text(face = "bold", family="Poppins", size = 25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank()
  )

ggsave(file.path(dir_rich, "Riqueza_especies_por_mes.png"),
       g_rich_mes, width = 10, height = 6,  dpi = 300)

# -------------------- HORIZONTAL --------------------

g_rich_mes_h <- ggplot(
  fenologia_riqueza_mes,
  aes(y = mes, x = riqueza)
) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Riqueza de espécies por mês",
    x = "Riqueza de espécies",
    y = "Mês"
  ) +
  #tema_minimal+
  theme(panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background  = element_rect(fill = "transparent", color = NA),
        axis.text =element_text(family="Poppins", size = 20),
        axis.line = element_line(color = "grey", linewidth = 0.5),
        axis.title = element_text(face = "bold", family="Poppins", size = 25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank()
  )

ggsave(file.path(dir_rich, "Riqueza_especies_por_mes_horizontal.png"),
       g_rich_mes_h, width = 10, height = 6, dpi = 300)


# ======================================================
# RIQUEZA DE ESPECIES POR ESTAÇÃO
# ======================================================

fenologia_riqueza_est <- galhas_inat %>%
  filter(!is.na(observed_on), !is.na(scientific_name)) %>%
  mutate(
    data = ymd(observed_on),
    mes_num = month(data),
    estacao = case_when(
      mes_num %in% c(12,1,2)   ~ "Inverno",
      mes_num %in% c(3,4,5)   ~ "Primavera",
      mes_num %in% c(6,7,8)   ~ "Verão",
      mes_num %in% c(9,10,11) ~ "Outono")) %>%
  group_by(estacao) %>%
  summarise(
    riqueza = n_distinct(scientific_name),
    .groups = "drop") %>%
  mutate(
    estacao = factor(estacao,
                     levels = c("Inverno","Primavera","Verão","Outono")))

# -------------------- BARPLOT --------------------
message("> A gerar barplot de riqueza por estação...")

# Garantir a ordem cronológica correta das 4 barras
estacoes_4 <- c("Inverno", "Primavera", "Verão", "Outono")
fenologia_riqueza_est <- fenologia_riqueza_est %>%
  complete(estacao = factor(estacoes_4, levels = estacoes_4), fill = list(riqueza = 0)) %>%
  mutate(est_num = match(estacao, estacoes_4)) %>%
  arrange(est_num)

# 2. Configurações de limites e backgrounds lineares
max_rich_est <- max(fenologia_riqueza_est$riqueza, na.rm = TRUE)
if(max_rich_est == 0) max_rich_est <- 1

# Dataframe para criar faixas verticais de fundo com as cores das estações
fondos_bar_est <- data.frame(
  xmin = (1:4) - 0.5,
  xmax = (1:4) + 0.5,
  cor  = c("#DCEEFF", "#E2F5D3", "#FFEAD2", "#F5E1ED")
)

# Posicionamento dos ícones logo abaixo do zero no eixo X
labels_iconos_bar <- data.frame(
  x = 1:4,
  y = - (max_rich_est * 0.10), # Posiciona ligeiramente abaixo da base da barra
  label = c(
    paste0("<img src='", path_inverno, "' width='", tamanho_icono, "'/>"),
    paste0("<img src='", path_primavera, "' width='", tamanho_icono, "'/>"),
    paste0("<img src='", path_verao, "' width='", tamanho_icono, "'/>"),
    paste0("<img src='", path_outono, "' width='", tamanho_icono, "'/>")
  )
)

# 3. Construção do Barplot Totalmente Limpo (Sem Eje Y)
g_rich_est <- ggplot() +
  # Layer 1: Fundos coloridos verticais bem suaves por trás de cada barra
  geom_rect(data = fondos_bar_est, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = max_rich_est * 1.15, fill = cor),
            alpha = 0.35, color = NA, inherit.aes = FALSE) +
  scale_fill_identity() +
  
  # Layer 2: Barras centrais com a mesma cor sólida e sem gradiente
  geom_col(data = fenologia_riqueza_est, aes(x = est_num, y = riqueza), 
           fill = "#2d6a4f", width = 0.7, color = "white", linewidth = 0.5) +
  
  # Layer 3: Valores numéricos na parte superior de cada barra
  geom_text(data = fenologia_riqueza_est, aes(x = est_num, y = riqueza, label = riqueza),
            family = "Poppins", fontface = "bold", size = 15, vjust = -0.5, color = "black") +
  
  # Layer 4: Íconos das estações no eixo X (substituindo o texto)
  ggtext::geom_richtext(data = labels_iconos_bar, aes(x = x, y = y, label = label), 
                        fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"), inherit.aes = FALSE) +
  
  # Customização de escalas (expandido para dar respiro aos textos e íconos)
  scale_x_continuous(breaks = 1:4, labels = NULL) + 
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.12))) + 
  
  # Aplicar o mesmo tema estrito do projeto eliminando por completo o eje Y
  tema_minimal +
  theme(
    axis.text.y  = element_blank(), # Elimina los números del eje Y
    axis.title.y = element_blank(), # Elimina el título del eje Y
    axis.ticks.y = element_blank(), # Elimina las marcas del eje Y
    axis.line.y  = element_blank()  # Elimina la línea del eje Y
  )

# Salvar o barplot linear modificado
ggsave(file.path(dir_rich, "Riqueza_especies_por_estacao.png"), g_rich_est, width = 9, height = 6, dpi = 300)

# -------------------- CIRCULAR --------------------
message("> A gerar riqueza por estação...")

# Garantir a ordem cronológica correta das 4 fatias no círculo
estacoes_4 <- c("Inverno", "Primavera", "Verão", "Outono")
fenologia_riqueza_est <- fenologia_riqueza_est %>%
  complete(estacao = factor(estacoes_4, levels = estacoes_4), fill = list(riqueza = 0)) %>%
  mutate(est_num = match(estacao, estacoes_4)) %>%
  arrange(est_num)

# 2. Definir limites e raios baseados na riqueza máxima desta seção
max_rich_est <- max(fenologia_riqueza_est$riqueza, na.rm = TRUE)
if(max_rich_est == 0) max_rich_est <- 1

r_max_rich_e        <- max_rich_est * 1.05
r_arco_est_min_e    <- max_rich_est * 1.14
r_arco_est_max_e    <- max_rich_est * 1.32
r_estaciones_iconos <- max_rich_est * 1.23

# Dataframes auxiliares para as 4 fatias perfeitas (sem divisões de meses)
quesitos_fundo_est <- data.frame(
  xmin = (1:4) - 0.5,
  xmax = (1:4) + 0.5,
  cor  = c("#DCEEFF", "#E2F5D3", "#FFEAD2", "#F5E1ED") # Inverno, Primavera, Verão, Outono
)

# Posicionamento dos ícones exatamente no centro de cada uma das 4 fatias
labels_estaciones_e <- data.frame(
  x = 1:4,
  y = rep(r_estaciones_iconos, 4),
  label = c(
    paste0("<img src='", path_inverno, "' width='", tamanho_icono, "'/>"),
    paste0("<img src='", path_primavera, "' width='", tamanho_icono, "'/>"),
    paste0("<img src='", path_verao, "' width='", tamanho_icono, "'/>"),
    paste0("<img src='", path_outono, "' width='", tamanho_icono, "'/>")
  )
)

# Linhas divisórias manuais (fatias exatas de 90º para cada estação)
divisorias_est <- data.frame(x = c(1.5, 2.5, 3.5, 4.5))

# 3. Construção do Gráfico de Riqueza por Estação
g_rich_est_polar <- ggplot() +
  # 1. Quesitos coloridos de fundo (Fundo suave para os dados centrais)
  geom_rect(data = quesitos_fundo_est, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = r_max_rich_e, fill = cor),
            alpha = 0.5, color = NA, inherit.aes = FALSE) +
  scale_fill_identity() +
  
  # 2. Barras centrais de dados (Gradiente verde clássico do projeto)
  ggnewscale::new_scale_fill() +
  geom_col(data = fenologia_riqueza_est, aes(x = est_num, y = riqueza, fill = riqueza), 
           width = 0.85, color = "white", linewidth = 0.4) +
  scale_fill_gradient(low = "#e0ecd1", high = "#2d6a4f") +
  
  # 3. Anel exterior colorido das estações (Onde ficam os ícones)
  ggnewscale::new_scale_fill() +
  geom_rect(data = quesitos_fundo_est, aes(xmin = xmin, xmax = xmax, ymin = r_arco_est_min_e, ymax = r_arco_est_max_e, fill = cor), 
            alpha = 0.9, color = NA, inherit.aes = FALSE) +
  scale_fill_identity() +
  
  # 4. Linhas divisórias brancas entre as 4 estações
  geom_segment(data = divisorias_est, aes(x = x, xend = x, y = r_arco_est_min_e, yend = r_arco_est_max_e), 
               color = "white", linewidth = 0.6, inherit.aes = FALSE) +
  
  # 5. Ícones das Estações no anel exterior (Sem nenhum texto ou título)
  ggtext::geom_richtext(data = labels_estaciones_e, aes(x = x, y = y, label = label), 
                        fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"), inherit.aes = FALSE) +
  
  coord_polar(start = 0) +
  scale_x_continuous(limits = c(0.5, 4.5), breaks = 1:4) +
  ylim(0, max_rich_est * 1.45) +
  tema_minimal +
  theme(panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background  = element_rect(fill = "transparent", color = NA))

# Salvar o gráfico finalizado na pasta de riqueza
ggsave(file.path(dir_rich, "Riqueza_especies_por_estacao_circular.png"), g_rich_est_polar, width = 12, height = 10, dpi = 300)







p1 <- ggplot(galhas_inat, aes(x = distance_to_sea, y = altitude)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", color = "red") +
  labs(x = "Distância ao mar (km)",
       y = "Altitude (m)",
       title = "Gradiente costa–altitude") +
  #tema_minimal +
  theme(panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background  = element_rect(fill = "transparent", color = NA),
        axis.text =element_text(family="Poppins", size = 20),
        axis.line = element_line(color = "grey", linewidth = 0.5),
        axis.title = element_text(face = "bold", family="Poppins", size = 25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank()
  )

ggsave(filename = file.path(dir_eco, "01_gradiente_global.png"),
       plot = p1,width = 10,height = 6,dpi = 300)

eco_space <- galhas_inat %>%
  filter(!is.na(altitude), !is.na(distance_to_sea)) %>%
  group_by(scientific_name) %>%
  summarise(
    alt_mean = mean(altitude),
    dist_mean = mean(distance_to_sea),
    alt_sd = sd(altitude),
    dist_sd = sd(distance_to_sea),
    .groups = "drop")

p2 <- ggplot(eco_space, aes(x = dist_mean, y = alt_mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = alt_mean - alt_sd, ymax = alt_mean + alt_sd)) +
  geom_errorbarh(aes(xmin = dist_mean - dist_sd, xmax = dist_mean + dist_sd)) +
  labs(x = "Distância média ao mar (km)",
       y = "Altitude média (m)",
       title = "Espaço ecológico por espécie") +
  #tema_minimal +
  theme(panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background  = element_rect(fill = "transparent", color = NA),
        axis.text =element_text(family="Poppins", size = 20),
        axis.line = element_line(color = "grey", linewidth = 0.5),
        axis.title = element_text(face = "bold", family="Poppins", size = 25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank()
  )

ggsave(filename = file.path(dir_eco, "02_espaco_ecologico_especies.png"),
       plot = p2, width = 10, height = 6, dpi = 300)



# ======================================================
# BIAS ESPACIAL DE MUESTREO (GLOBAL + POR ESPECIE)
# ======================================================
# 1. BIAS ESPACIAL GLOBAL (ESFUERZO DE MUESTREO)
# ======================================================

p_bias_global <- ggplot(galhas_inat, aes(x = longitude, y = latitude)) +
  stat_density_2d_filled(alpha = 0.7) +
  labs(title = "Bias espacial de muestreo (global)",
       x = "Longitude",
       y = "Latitude") +
  #tema_minimal +
  theme(axis.text =element_text(family="Poppins", size = 20),
        axis.line = element_line(color = "grey", linewidth = 0.5),
        axis.title = element_text(face = "bold", family="Poppins", size = 25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        #legend.position = "none",
        panel.background = element_blank(),
        axis.ticks = element_blank()
  )

ggsave(file.path(dir_eco, "bias_global_densidad.png"),
       p_bias_global,width = 10,height = 7,dpi = 300)

# ======================================================
# 2. CENTRO DE MUESTREO (GLOBAL)
# ======================================================

centro <- galhas_inat %>%
  summarise(lon = mean(longitude, na.rm = TRUE),
            lat = mean(latitude, na.rm = TRUE))

# ======================================================
# 3. DISTANCIA AL CENTRO (BIAS POR REGISTRO)
# ======================================================

galhas_bias <- galhas_inat %>%
  mutate(dist_centro = sqrt((longitude - centro$lon)^2 +
                              (latitude - centro$lat)^2))

# ======================================================
# 4. BIAS POR ESPECIE (ÍNDICE CUANTITATIVO)
# ======================================================

bias_species <- galhas_bias %>%
  group_by(scientific_name) %>%
  summarise(bias_media = mean(dist_centro, na.rm = TRUE),
            bias_sd = sd(dist_centro, na.rm = TRUE),
            n = n(),
            .groups = "drop") %>%
  arrange(bias_media) %>%
  mutate(scientific_name = factor(scientific_name, levels = scientific_name))

# ======================================================
# 5. GRÁFICO: BIAS POR ESPECIE
# ======================================================

p_bias_species <- ggplot(bias_species, aes(x = scientific_name, y = bias_media)) +
  geom_point(size = 2.5, color = "#1B4965") +
  geom_errorbar(aes(ymin = bias_media - bias_sd, ymax = bias_media + bias_sd), width = 0.2, color = "grey50") +
  coord_flip() +
  labs(title = "Bias espacial por espécie",
       x = "Espécie",
       y = "Distância média ao centro de amostragem") +
  #tema_minimal +
  theme(axis.text.y =element_text(face = "italic", family="Poppins", size = 5),
        axis.text.x =element_text(family="Poppins", size = 20),
        axis.line = element_line(color = "grey", linewidth = 0.5),
        axis.title = element_text(face = "bold", family="Poppins", size = 25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.ticks = element_blank()
  )

ggsave(file.path(dir_eco, "bias_por_especie.png"),
       p_bias_species, width = 10, height = 7, dpi = 300)

# ======================================================
# 6. DISTRIBUIÇÃO DEL BIAS (GLOBAL)
# ======================================================

p_bias_hist <- ggplot(galhas_bias, aes(x = dist_centro)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Distribuição del bias espacial",
       x = "Distância ao centro de amostragem",
       y = "Densidade") +
  #tema_minimal +
  theme(axis.text =element_text(family="Poppins", size = 20),
        axis.line = element_line(color = "grey", linewidth = 0.5),
        axis.title = element_text(face = "bold", family="Poppins", size = 25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        #legend.position = "none",
        panel.background = element_blank(),
        axis.ticks = element_blank()
  )

ggsave(file.path(dir_eco, "bias_histograma_global.png"),
       p_bias_hist, width = 10, height = 6,  dpi = 300)




# ======================================================
# ANÁLISE QUANTITATIVA DE BIAS ESPACIAL E DE UTILIZADORES
# iNaturalist / Dados de ocorrência (DATASET GLOBAL)
# ======================================================
# 2. CENTRO GLOBAL DE AMOSTRAGEM
# ======================================================

centro <- galhas_inat %>%
  summarise(
    lon = mean(longitude, na.rm = TRUE),
    lat = mean(latitude, na.rm = TRUE))
print(centro)

# ======================================================
# 3. DISTÂNCIA AO CENTRO DE AMOSTRAGEM
# ======================================================

galhas_bias <- galhas_inat %>%
  mutate(dist_centro = sqrt((longitude - centro$lon)^2 +
                              (latitude - centro$lat)^2))

# ======================================================
# 4. BIAS ESPACIAL POR ESPÉCIE
# ======================================================

bias_species <- galhas_bias %>%
  group_by(scientific_name) %>%
  summarise(n_registos = n(),
            bias_medio = mean(dist_centro, na.rm = TRUE),
            bias_sd = sd(dist_centro, na.rm = TRUE),
            dist_min = min(dist_centro, na.rm = TRUE),
            dist_max = max(dist_centro, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(bias_medio)
print(bias_species)

# ======================================================
# 5. EXPORTAR TABELA DE BIAS POR ESPÉCIE
# ======================================================

write.csv2(bias_species,
           file = file.path(base_dir, "Bias_espacial_por_especie.csv"),
           row.names = FALSE)

# ======================================================
# 6. NEAREST-NEIGHBOUR DISTANCE (GLOBAL)
# ======================================================

coords <- galhas_inat %>%
  select(longitude, latitude)

pp <- ppp(x = coords$longitude, y = coords$latitude, window = owin(xrange = range(coords$longitude), yrange = range(coords$latitude)))
nn_dist <- mean(nndist(pp))

cat("\n")
cat("=====================================\n")
cat("NEAREST-NEIGHBOUR DISTANCE\n")
cat("=====================================\n")
cat("Distância média ao vizinho mais próximo:", nn_dist, "\n")

# ======================================================
# 7. MORAN'S I (AUTOCORRELAÇÃO ESPACIAL)
# ======================================================

coords_matrix <- cbind( galhas_inat$longitude, galhas_inat$latitude)

# vizinhos mais próximos
nb <- knn2nb(knearneigh(coords_matrix, k = 5))
lw <- nb2listw(nb)
moran_result <- moran.test(galhas_bias$dist_centro, lw)

cat("\n")
cat("=====================================\n")
cat("MORAN'S I\n")
cat("=====================================\n")

print(moran_result)

# ======================================================
# 8. BIAS DE UTILIZADORES
# ======================================================

# verificar se existe coluna de utilizador
if("user_login" %in% names(galhas_inat)) {
  
  user_bias <- galhas_inat %>%
    filter(!is.na(user_login)) %>%
    group_by(user_login) %>%
    summarise(n_registos = n(),
              .groups = "drop") %>%
    arrange(desc(n_registos))
  
  # top 10 utilizadores
  top10_prop <- sum(head(user_bias$n_registos, 10)) /
    sum(user_bias$n_registos)
  
  cat("\n")
  cat("=====================================\n")
  cat("BIAS DE UTILIZADORES\n")
  cat("=====================================\n")
  
  cat("Número total de utilizadores:",
      nrow(user_bias), "\n")
  
  cat("Proporção dos 10 utilizadores mais ativos:",
      round(top10_prop * 100, 2), "%\n")
  
  # exportar tabela
  write.csv2(user_bias,
             file = file.path(base_dir, "Bias_utilizadores.csv"),
             row.names = FALSE)
}

# ======================================================
# 9. COBERTURA ESPACIAL (GRID)
# ======================================================

galhas_sf <- st_as_sf(galhas_inat,
                      coords = c("longitude", "latitude"),
                      crs = 4326)

# criar grid
grid <- st_make_grid(galhas_sf,
                     cellsize = 0.1)
# interseções
grid_occ <- st_intersects(grid, galhas_sf)
n_cells <- sum(lengths(grid_occ) > 0)

cat("\n")
cat("=====================================\n")
cat("COBERTURA ESPACIAL\n")
cat("=====================================\n")

cat("Número de células ocupadas:", n_cells, "\n")

# ======================================================
# 10. RESUMO FINAL
# ======================================================

cat("\n")
cat("=====================================\n")
cat("RESUMO FINAL\n")
cat("=====================================\n")

cat("✔ Bias espacial por espécie calculado\n")
cat("✔ Nearest-neighbour distance calculado\n")
cat("✔ Moran's I calculado\n")
cat("✔ Bias de utilizadores calculado\n")
cat("✔ Cobertura espacial calculada\n")


sink(file.path(base_dir, "Resumo_bias_quantitativo.txt"))

cat("=====================================\n")
cat("ANÁLISE QUANTITATIVA DE BIAS\n")
cat("=====================================\n\n")
cat("Nearest-neighbour distance:\n")
print(nn_dist)
cat("\n\nMoran's I:\n")
print(moran_result)

if(exists("top10_prop")) {
  cat("\n\nBias de utilizadores:\n")
  cat("Top 10 utilizadores contribuíram com ",
      round(top10_prop * 100, 2),
      "% dos registos.\n",sep = "")
}

cat("\n\nCobertura espacial:\n")
cat("Número de células ocupadas:",n_cells,"\n")
sink()

cat("\n✔ Resumo exportado com sucesso\n")


# ======================================================
# ANÁLISE DE BIAS ESPACIAL POR ESPÉCIE
# iNaturalist / Dados de ocorrência
# ======================================================

species_list <- sort(unique(galhas_inat$scientific_name))
bias_results <- data.frame()

for(sp in species_list) {
  
  cat("\n")
  cat("=====================================\n")
  cat("Espécie:", sp, "\n")
  cat("=====================================\n")
  # -------------------- dados da espécie --------------------
  sp_data <- galhas_inat %>%
    filter(scientific_name == sp)
  # mínimo de pontos
  if(nrow(sp_data) < 5) {
    cat("Poucos registos -> ignorada\n")
    next
  }
  
  # ======================================================
  # 1. NÚMERO DE REGISTOS
  # ======================================================
  
  n_records <- nrow(sp_data)
  
  # ======================================================
  # 2. NÚMERO DE UTILIZADORES
  # ======================================================
  
  if("user_login" %in% names(sp_data)) {
    n_users <- length(unique(sp_data$user_login))
    user_table <- sp_data %>%
      group_by(user_login) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(desc(n))
    top_user_prop <- max(user_table$n) / sum(user_table$n)
  } else {
    n_users <- NA
    top_user_prop <- NA
  }
  
  # ======================================================
  # 3. NEAREST-NEIGHBOUR DISTANCE
  # ======================================================
  
  coords <- sp_data %>%
    select(longitude, latitude)
  pp <- ppp(x = coords$longitude, y = coords$latitude, window = owin(xrange = range(coords$longitude), yrange = range(coords$latitude)))
  nn_distance <- mean(nndist(pp))
  
  # ======================================================
  # 4. MORAN'S I
  # ======================================================
  
  moran_I <- NA
  moran_p <- NA
  
  if(nrow(coords) > 5) {
    coords_matrix <- cbind(coords$longitude, coords$latitude)
    try({
      nb <- knn2nb(knearneigh(coords_matrix, k = 5))
      lw <- nb2listw(nb)
      values <- seq_len(nrow(coords_matrix))
      moran_test <- moran.test(values, lw)
      moran_I <- moran_test$estimate[1]
      moran_p <- moran_test$p.value}, silent = TRUE)
  }
  
  # ======================================================
  # 5. COBERTURA ESPACIAL
  # ======================================================
  
  sp_sf <- st_as_sf(sp_data,
                    coords = c("longitude", "latitude"),
                    crs = 4326)
  grid <- st_make_grid(sp_sf, cellsize = 0.1)
  inters <- st_intersects(grid, sp_sf)
  occupied_cells <- sum(lengths(inters) > 0)
  
  # ======================================================
  # 6. ÁREA GEOGRÁFICA APROXIMADA
  # ======================================================
  
  lon_range <- diff(range(sp_data$longitude))
  lat_range <- diff(range(sp_data$latitude))
  
  geo_extent <- lon_range * lat_range
  
  # ======================================================
  # 7. GUARDAR RESULTADOS
  # ======================================================
  
  bias_results <- rbind(bias_results, data.frame(
    species = sp,
    n_records = n_records,
    n_users = n_users,
    top_user_prop = top_user_prop,
    nearest_neighbour = nn_distance,
    moran_I = moran_I,
    moran_p = moran_p,
    occupied_cells = occupied_cells,
    geo_extent = geo_extent))
}

# ======================================================
# ORDENAR RESULTADOS
# ======================================================

bias_results <- bias_results %>%
  arrange(desc(moran_I))

# ======================================================
# EXPORTAR CSV
# ======================================================

write.csv2(bias_results,file.path(dir_bias, "Bias_por_especie.csv"),row.names = FALSE)

# ======================================================
# GRÁFICO 1 — MORAN'S I
# ======================================================

g1 <- ggplot(bias_results, aes(x = reorder(species, moran_I),y = moran_I)) +
  geom_col(fill = "#2E86AB") +
  coord_flip() +
  labs(title = "Autocorrelação espacial por espécie",
       x = "Espécie",
       y = "Moran's I") +
  tema_minimal

ggsave(file.path(dir_bias, "MoranI_por_especie.png"),
       g1,width = 10,height = 8,dpi = 300)

# ======================================================
# GRÁFICO 2 — NEAREST NEIGHBOUR
# ======================================================

g2 <- ggplot(bias_results,aes(x = reorder(species, nearest_neighbour),y = nearest_neighbour)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Nearest-neighbour distance por espécie",
       x = "Espécie",
       y = "Distância média") +
  tema_minimal

ggsave(file.path(dir_bias, "NearestNeighbour_por_especie.png"),
       g2,width = 10,height = 8, dpi = 300)

# ======================================================
# GRÁFICO 3 — BIAS DE UTILIZADORES
# ======================================================

g3 <- ggplot(bias_results, aes(x = reorder(species, top_user_prop), y = top_user_prop)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Dominância do principal utilizador",
       x = "Espécie",
       y = "Proporção") +
  tema_minimal

ggsave(file.path(dir_bias, "Bias_utilizadores_por_especie.png"),
       g3,width = 10,height = 8,dpi = 300)

# ======================================================
# GRÁFICO 4 — COBERTURA ESPACIAL
# ======================================================

g4 <- ggplot(bias_results, aes(x = reorder(species, occupied_cells), y = occupied_cells)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(title = "Cobertura espacial por espécie",
       x = "Espécie",
       y = "Número de células ocupadas") +
  tema_minimal

ggsave(file.path(dir_bias, "Cobertura_espacial_por_especie.png"),
       g4,width = 10,height = 8, dpi = 300)

# ======================================================
# RESUMO FINAL
# ======================================================

cat("\n")
cat("=====================================\n")
cat("ANÁLISE TERMINADA\n")
cat("=====================================\n")
cat("Número de espécies analisadas:",
    nrow(bias_results), "\n")
cat("Resultados exportados para:\n")
cat(dir_bias, "\n")



# ======================================================
# FINAL
# ======================================================

message("------------------------------------------------")
message("✓ Todos os gráficos foram gerados")
message("------------------------------------------------")