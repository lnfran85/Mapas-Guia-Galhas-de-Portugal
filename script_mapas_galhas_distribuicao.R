# -------------------- PACKAGES --------------------
library(sf)
library(ggplot2)
library(dplyr)
library(ggtext)
library(showtext)
library(sysfonts)
font_add_google("Poppins")
showtext::showtext_auto()

# -------------------- CONFIGURACIÓN DE RUTAS (SIN ALTERAR) --------------------
path_csv       <- "./occur_data/observations-747569.csv/observations-747569.csv"
path_grid      <- "./Input_shp/utm10+ilhas_mod.gpkg"
path_distritos <- "./Input_shp/CAOP_2024_distritos/Distritos_dissolvidos.shp"
path_muni      <- "./Input_shp/CAOP_2024_distritos/Municipios_dissolvidos.shp" 

# Creación de carpetas
sapply(c("Outputs", "Input_shp", "occur_data"), function(x) if(!dir.exists(x)) dir.create(x))

crs_ref <- 4326

# -------------------- FUNCIONES DE APOYO --------------------
safe_filename <- function(x) {
  x %>% gsub("×", "x", .) %>% gsub("[^A-Za-z0-9_\\-]", "_", .) %>% 
    gsub("_+", "_", .) %>% gsub("^_|_$", "", .)
}

reposicionar_geometria <- function(sf_obj) {
  sf_obj <- st_transform(sf_obj, crs_ref)
  cont <- sf_obj %>% filter(grepl("Continente", nuts1, ignore.case = TRUE))
  mad  <- sf_obj %>% filter(grepl("Madeira", nuts1, ignore.case = TRUE))
  aco  <- sf_obj %>% filter(grepl("Açores", nuts1, ignore.case = TRUE))
  
  if(nrow(mad) > 0) {
    geom_mad <- st_geometry(mad) + c(5, 6)
    mad <- st_set_geometry(mad, geom_mad)
    st_crs(mad) <- crs_ref
  }
  if(nrow(aco) > 0) {
    geom_aco <- st_geometry(aco) + c(16.5, 2.8)
    centro_aco <- st_centroid(st_union(geom_aco))
    geom_aco <- (geom_aco - centro_aco) * 0.5714 + centro_aco
    aco <- st_set_geometry(aco, geom_aco)
    st_crs(aco) <- crs_ref
  }
  res <- rbind(cont, aco, mad)
  st_crs(res) <- crs_ref
  return(res)
}

# -------------------- CARGA Y PRE-PROCESADO --------------------
message("> Cargando capas geográficas...")

grid_base      <- st_read(path_grid, quiet = TRUE) %>% st_make_valid() %>% st_transform(crs_ref)
distritos_base <- st_read(path_distritos, quiet = TRUE) %>% st_make_valid() %>% st_transform(crs_ref)
muni_base      <- st_read(path_muni, quiet = TRUE) %>% st_make_valid() %>% st_transform(crs_ref)

galhas_inat <- read.csv(path_csv) %>%
  mutate(lat = ifelse(!is.na(private_latitude) & private_latitude != "", private_latitude, latitude),
         lon = ifelse(!is.na(private_longitude) & private_longitude != "", private_longitude, longitude)) %>% 
  filter(!is.na(lat), !is.na(lon))

# Versiones Visuales Reposicionadas
grid_visual      <- reposicionar_geometria(grid_base)
distritos_visual <- reposicionar_geometria(distritos_base)
muni_visual      <- reposicionar_geometria(muni_base)

linha_sep <- st_sfc(st_linestring(rbind(c(-13.3, 41.0), c(-10.5, 39.5))), crs = crs_ref) %>% st_sf()
col_raridade <- c("Muito restrita" = "#B2182B", "Restrita" = "#EF8A62", "Moderada" = "#FDAE61", "Ampla" = "#91CF60", "Muito ampla" = "#4575B4")

# -------------------- CÁLCULO DE DENOMINADORES TOTALES --------------------
# Cuadrículas únicas reales
df_grid_unico  <- st_drop_geometry(grid_base) %>% distinct(utm10, .keep_all = TRUE)
total_grid_val <- nrow(df_grid_unico)

# Distritos y Municipios totales reales
total_dist_val <- length(unique(distritos_base$Distrito))
total_muni_val <- length(unique(muni_base$Municipio))

# -------------------- INICIALIZAR TABLA RESUMEN --------------------
lista_resumen <- list()

# -------------------- LOOP POR ESPECIE (TOP 2) --------------------
species_list <- sort(unique(galhas_inat$scientific_name))
#top_2_species <- head(species_list, 2)

for (sp in species_list) {
  message("--- Procesando: ", sp)
  sp_safe <- safe_filename(sp)
  pts_sp <- galhas_inat %>% filter(scientific_name == sp) %>% st_as_sf(coords = c("lon", "lat"), crs = crs_ref)
  
  # 1. OCUPACIÓN POR CUADRÍCULAS (UTM10)
  pts_join <- st_join(pts_sp, grid_base %>% select(utm10), join = st_intersects)
  utms_ocupadas <- unique(na.omit(pts_join$utm10))
  n_quad <- length(utms_ocupadas)
  
  if(n_quad == 0) next
  
  categoria <- case_when(
    n_quad == 1 ~ "Muito restrita",
    n_quad <= 5 ~ "Restrita",
    n_quad <= 25 ~ "Moderada",
    n_quad <= 125 ~ "Ampla",
    TRUE ~ "Muito ampla"
  )
  
  # 2. OCUPACIÓN ADMINISTRATIVA (DISTRITOS Y MUNICIPIOS)
  dist_pres_list <- sort(unique(distritos_base$Distrito[as.logical(lengths(st_intersects(distritos_base, pts_sp)))]))
  muni_pres_list <- sort(unique(muni_base$Municipio[as.logical(lengths(st_intersects(muni_base, pts_sp)))]))
  
  n_distritos  <- length(dist_pres_list)
  n_municipios <- length(muni_pres_list)
  
  # 3. GUARDAR DATOS EN LA LISTA
  lista_resumen[[sp]] <- data.frame(
    especie = sp,
    quadrículas_ocupadas = n_quad,
    quadrículas_totales = total_grid_val,
    raridade = categoria,
    n_distritos_presencia = n_distritos,
    total_distritos_capa = total_dist_val,
    distritos_nombres = paste(dist_pres_list, collapse = ", "),
    n_municipios_presencia = n_municipios,
    total_municipios_capa = total_muni_val,
    municipios_nombres = paste(muni_pres_list, collapse = ", "),
    stringsAsFactors = FALSE
  )
  
  # 4. PREPARACIÓN DE CAPAS VISUALES
  grid_plot_data <- grid_visual %>% mutate(presente = utm10 %in% utms_ocupadas)
  dist_plot_data <- distritos_visual %>% mutate(presenca = Distrito %in% dist_pres_list)
  muni_plot_data <- muni_visual %>% mutate(presenca = Municipio %in% muni_pres_list)
  
  # 5. TEXTOS DINÁMICOS
  subtitle_text <- paste0(
    "Ocupação: ", n_quad, "/", total_grid_val, " Quadrículas | ",
    n_distritos, "/", total_dist_val, " Distritos | ",
    n_municipios, "/", total_muni_val, " Municípios"
  )
  fecha_hoy <- format(Sys.Date(), "%d-%m-%Y")
  texto_caption <- paste0("Dados: iNaturalist (", fecha_hoy, ") | Portugal Continental e Ilhas")
  
  # --- MAPA 1: DISTRITOS ---
  m1 <- ggplot() +
    geom_sf(data = dist_plot_data, aes(fill = presenca), color = "white", linewidth = 0.1) +
    geom_sf(data = linha_sep, color = "grey50", linewidth = 0.4) +
    scale_fill_manual(values = c("TRUE" = "#142840", "FALSE" = "lightgrey"), labels = c("TRUE" = "Presente", "FALSE" = "Ausente"), name = "") +
    labs(title = bquote(paste("", italic(.(sp)))), subtitle = subtitle_text, caption = texto_caption, x = NULL, y = NULL, family = "Poppins") +
    geom_richtext(aes(x = -Inf, y = Inf, label = paste0("Raridade: ", "<span style='color:", col_raridade[categoria], ";'>", categoria, "</span>")),
                                  hjust = 0, vjust = 1, fill = "#f5f5f5", alpha= 0.7, label.color = NA, size = 12, family="Poppins") +
    coord_sf(expand = FALSE) +
    theme_minimal() + theme(legend.position = "bottom", axis.text = element_blank(), panel.grid = element_blank()) +
    theme(text=element_text(family="Poppins", size = 35), axis.text = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          legend.position = "bottom",
          # 1. Justifica la leyenda hacia la parte superior de su contenedor
          legend.justification = "top", 
          # 2. Elimina o reduce los márgenes para pegarla más arriba (t, r, b, l = top, right, bottom, left)
          legend.margin = margin(t = -100, r = 0, b = 0, l = 0, unit = "pt"),
          #plot.margin = margin(t = 10, r = -100, b = 10, l = -100, unit = "pt"),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background  = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          plot.caption = element_text(hjust = 0.5))
  ggsave(file.path("Outputs", paste0("1_Distritos_", sp_safe, ".png")), m1, width = 9.5, height = 9, dpi = 300, bg = "transparent")
  
  # --- MAPA 2: MUNICIPIOS ---
  m2 <- ggplot() +
    geom_sf(data = muni_plot_data, aes(fill = presenca), color = "white", linewidth = 0.05) +
    geom_sf(data = linha_sep, color = "grey50", linewidth = 0.4) +
    scale_fill_manual(values = c("TRUE" = "#142840", "FALSE" = "lightgrey"), labels = c("TRUE" = "Presente", "FALSE" = "Ausente"), name = "") +
    labs(title = bquote(paste("", italic(.(sp)))), subtitle = subtitle_text, caption = texto_caption, x = NULL, y = NULL, family = "Poppins") +
    geom_richtext(aes(x = -Inf, y = Inf, label = paste0("Raridade: ", "<span style='color:", col_raridade[categoria], ";'>", categoria, "</span>")),
                                  hjust = 0, vjust = 1, fill = "#f5f5f5", alpha= 0.7, label.color = NA, size = 12, family = "Poppins") +
    coord_sf(expand = FALSE) +
    theme_minimal() + theme(legend.position = "bottom", axis.text = element_blank(), panel.grid = element_blank())+
    theme(text=element_text(family="Poppins", size = 35), axis.text = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          # 1. Justifica la leyenda hacia la parte superior de su contenedor
          legend.justification = "top", 
          # 2. Elimina o reduce los márgenes para pegarla más arriba (t, r, b, l = top, right, bottom, left)
          legend.margin = margin(t = -100, r = 0, b = 0, l = 0, unit = "pt"),
          #plot.margin = margin(t = 10, r = -100, b = 10, l = -100, unit = "pt"),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background  = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          plot.caption = element_text(hjust = 0.5))
  ggsave(file.path("Outputs", paste0("2_Municipios_", sp_safe, ".png")), m2, width = 9.5, height = 9, dpi = 300, bg = "transparent")
  
  # --- MAPA 3: GRID UTM ---
  #m3 <- ggplot() +
  #  geom_sf(data = grid_plot_data, aes(fill = presente), color = "grey50", linewidth = 0.1) +
  #  geom_sf(data = linha_sep, color = "grey50", linewidth = 0.4) +
  #  scale_fill_manual(values = c("TRUE" = "darkkhaki", "FALSE" = "lightgrey"), guide = "none") +
  #  labs(title = bquote(paste("Distribución UTM 10x10km: ", italic(.(sp)))), subtitle = subtitle_text) +
  #  geom_richtext(aes(x = -Inf, y = Inf, label = paste0("Raridade: ", "<span style='color:", col_raridade[categoria], ";'>", categoria, "</span>")),
  #                hjust = 0, vjust = 1, fill = "#f5f5f5", alpha= 0.7, label.color = NA, size = 3.5) +
  #  theme_minimal() + theme(axis.text = element_blank(), panel.grid = element_blank())
  #ggsave(file.path("Outputs", paste0("3_GridPuro_", sp_safe, ".png")), m3, width = 16, height = 9, dpi = 300)
  
  
  
  # --- MAPA 4: COMBINADO (GRID + LÍMITES DISTRITALES) ---
  #m4 <- ggplot() +
  #  geom_sf(data = grid_plot_data, aes(fill = presente), color = "grey70") +
  #  geom_sf(data = distritos_visual, fill = NA, color = "grey40", linewidth = 0.3) +
  #  geom_sf(data = linha_sep, color = "grey50", linewidth = 0.4) +
  #  scale_fill_manual(values = c("TRUE" = "#142840", "FALSE" = "lightgrey"), 
  #                    labels = c("TRUE" = "Com presença", "FALSE" = "Sem presença"), name = "") +
  #  labs(title = bquote(paste("", italic(.(sp)))), 
  #       subtitle = subtitle_text, caption = texto_caption, x = NULL, y = NULL, family = "Poppins") +
  #  coord_sf(expand = FALSE) +
  #  geom_richtext(aes(x = -Inf, y = Inf, label = paste0("Frequência: ", "<span style='color:", col_raridade[categoria], "; font-weight:bold;'>", categoria, "</span>")),
  #                hjust = 0, vjust = 1, fill = "#f5f5f5", alpha= 0.7, label.color = NA, size = 12, family = "Poppins") +
  #  theme_minimal() + theme(legend.position = "bottom", axis.text = element_blank(), panel.grid = element_blank(), 
  #                          plot.title = element_text(hjust = 0, size = 14, face = "bold"))+
  #  theme(text=element_text(family="Poppins", size = 35), axis.text = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
  #       axis.title.x = element_blank(), axis.title.y = element_blank(),
         # 1. Justifica la leyenda hacia la parte superior de su contenedor
  #       legend.justification = "top", 
         # 2. Elimina o reduce los márgenes para pegarla más arriba (t, r, b, l = top, right, bottom, left)
  #       legend.margin = margin(t = -100, r = 0, b = 0, l = 0, unit = "pt"),
         #plot.margin = margin(t = 10, r = -100, b = 10, l = -100, unit = "pt"),
  #       panel.background = element_rect(fill = "transparent", color = NA),
  #       plot.background  = element_rect(fill = "transparent", color = NA),
  #       legend.background = element_rect(fill = "transparent", color = NA),
  #       plot.caption = element_text(hjust = 0.5))
  #ggsave(file.path("Outputs", paste0("4_Grid_Sobre_Distritos_", sp_safe, ".png")), m4, width = 9.5, height = 9, dpi = 300, bg = "transparent")
  
  message("✔ Finalizado: ", sp, " [", n_quad, " UTMs | ", n_distritos, " Distritos]")
}

# -------------------- GUARDAR TABLA FINAL --------------------
resumen_final <- bind_rows(lista_resumen)
write.csv(resumen_final, file.path("Outputs", "Resumen_Distribucion_Agallas_Completo.csv"), row.names = FALSE)

message("✔ Proceso completado exitosamente. Revisa la carpeta /Outputs.")
