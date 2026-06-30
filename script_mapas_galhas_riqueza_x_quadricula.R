# -------------------- PACKAGES --------------------
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(showtext)
library(sysfonts)
font_add_google("Poppins")
showtext::showtext_auto()

# -------------------- CONFIGURACIÓN DE RUTAS --------------------
path_csv        <- "./occur_data/observations-747569.csv/observations-747569.csv"
path_grid       <- "./Input_shp/utm10+ilhas_mod.gpkg"
path_distritos  <- "./Input_shp/CAOP_2024_distritos/Distritos_dissolvidos.shp"
path_muni       <- "./Input_shp/CAOP_2024_distritos/Municipios_dissolvidos.shp" 

if(!dir.exists("Outputs")) dir.create("Outputs")
crs_ref <- 4326

# -------------------- FUNCIÓN REPOSICIONAR (ISLAS) --------------------
reposicionar_geometria <- function(sf_obj) {
  # Asegurar que estamos en el CRS de trabajo para desplazar coordenadas
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
  return(res)
}

# -------------------- CARGA DE DATOS --------------------
message("> Cargando capas base...")
grid_base      <- st_read(path_grid, quiet = TRUE) %>% st_make_valid()
distritos_base <- st_read(path_distritos, quiet = TRUE) %>% st_make_valid()
muni_base      <- st_read(path_muni, quiet = TRUE) %>% st_make_valid()

message("> Procesando CSV de iNaturalist...")
galhas_inat <- read.csv(path_csv) %>%
  mutate(lat = ifelse(!is.na(private_latitude) & private_latitude != "", private_latitude, latitude),
         lon = ifelse(!is.na(private_longitude) & private_longitude != "", private_longitude, longitude)) %>% 
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = crs_ref) %>%
  # Alinear CRS inmediatamente para que la intersección sea ultra rápida
  st_transform(st_crs(grid_base))

# -------------------- CÁLCULO DE RIQUEZA (ÚNICAS) --------------------
# Esta función es la que optimiza el tiempo y asegura que sean especies ÚNICAS
calcular_riqueza_unicas <- function(capa_poligonal, puntos) {
  message(paste("  - Procesando:", deparse(substitute(capa_poligonal))))
  
  # st_intersects devuelve una lista de qué puntos están en cada polígono
  indices <- st_intersects(capa_poligonal, puntos)
  
  # n_distinct garantiza que contamos especies sin repetir (Riqueza)
  capa_poligonal$riqueza <- sapply(indices, function(x) {
    if(length(x) == 0) return(0)
    return(n_distinct(puntos$scientific_name[x], na.rm = TRUE))
  })
  return(capa_poligonal)
}

riqueza_grid <- calcular_riqueza_unicas(grid_base, galhas_inat)
riqueza_dist <- calcular_riqueza_unicas(distritos_base, galhas_inat)
riqueza_muni <- calcular_riqueza_unicas(muni_base, galhas_inat)

# -------------------- PREPARACIÓN VISUAL --------------------
message("> Reposicionando geometrías para visualización...")
grid_vis  <- reposicionar_geometria(riqueza_grid)
dist_vis  <- reposicionar_geometria(riqueza_dist)
muni_vis  <- reposicionar_geometria(riqueza_muni)

linha_sep <- st_sfc(st_linestring(rbind(c(-13.3, 41.0), c(-10.5, 39.5))), crs = crs_ref) %>% st_sf()

# -------------------- GENERACIÓN DE MAPAS --------------------
fecha_info <- format(Sys.Date(), "%d/%m/%Y")
texto_caption <- paste0("Dados: iNaturalist | Portugal Continental e Ilhas")
message("> Generando mapas...")
max_riqueza <- max(dist_vis$riqueza, na.rm = TRUE)
# MAPA 1: Distrito
m1 <- ggplot(dist_vis) +
  geom_sf(aes(fill = riqueza), color = "white", linewidth = 0.1) +
  geom_sf(data = linha_sep, color = "grey30", linewidth = 0.5) +
  #scale_fill_viridis_c(option = "viridis", name = "Nº Espécies") +
  #scale_fill_viridis_c(option = "magma", name = "Nº Espécies", direction = -1,
  #                     # Genera divisiones automáticas bonitas y añade el máximo real exacto
  #                     breaks = function(x) unique(c(scales::breaks_extended()(x), max_riqueza))) +
  scale_fill_viridis_c(option = "magma", name = "Nº Espécies", direction = -1) +
  labs(title = "Riqueza de espécies de galhas por Distrito", subtitle = paste("Actualizado:", fecha_info), caption = texto_caption, family = "Poppins") +
  theme_minimal() + theme(text=element_text(family="Poppins", size = 35), axis.text = element_blank(), panel.grid = element_blank(),
                          panel.background = element_rect(fill = "transparent", color = NA),
                          plot.background  = element_rect(fill = "transparent", color = NA),
                          legend.background = element_rect(fill = "transparent", color = NA),
                          plot.caption = element_text(hjust = 0.5))
ggsave("./Outputs/Riqueza_Distritos.png", m1, width = 9.5, height = 9, dpi = 300, bg = "transparent")

# MAPA 2: Municipio
m2 <- ggplot(muni_vis) +
  geom_sf(aes(fill = riqueza), color = "white", linewidth = 0.05) +
  geom_sf(data = linha_sep, color = "grey30", linewidth = 0.5) +
  #scale_fill_viridis_c(option = "magma", name = "Nº Espécies") +
  scale_fill_viridis_c(option = "magma", name = "Nº Espécies", direction = -1) +
  labs(title = "Riqueza de espécies de galhas por Município", subtitle = paste("Actualizado:", fecha_info), caption = texto_caption, family = "Poppins") +
  theme_minimal() + theme(text=element_text(family="Poppins", size = 35), axis.text = element_blank(), panel.grid = element_blank(),
                          panel.background = element_rect(fill = "transparent", color = NA),
                          plot.background  = element_rect(fill = "transparent", color = NA),
                          legend.background = element_rect(fill = "transparent", color = NA),
                          plot.caption = element_text(hjust = 0.5))
ggsave("./Outputs/Riqueza_Municipios.png", m2, width = 9.5, height = 9, dpi = 300, bg = "transparent")

# MAPA 3: Cuadrícula
m3 <- ggplot(grid_vis) +
  geom_sf(aes(fill = riqueza), color = NA) +
  geom_sf(data = linha_sep, color = "grey30", linewidth = 0.5) +
  #scale_fill_viridis_c(option = "plasma", name = "Nº Espécies") +
  scale_fill_viridis_c(option = "magma", name = "Nº Espécies", direction = -1) +
  labs(title = "Riqueza de espécies de galhas por Quadrícula UTM 10x10km", subtitle = paste("Actualizado:", fecha_info), caption = texto_caption, family = "Poppins") +
  theme_minimal() + theme(text=element_text(family="Poppins", size = 35), axis.text = element_blank(), panel.grid = element_blank(),
                          panel.background = element_rect(fill = "transparent", color = NA),
                          plot.background  = element_rect(fill = "transparent", color = NA),
                          legend.background = element_rect(fill = "transparent", color = NA),
                          plot.caption = element_text(hjust = 0.5))
ggsave("./Outputs/Riqueza_Grid_Puro.png", m3, , width = 9.5, height = 9, dpi = 300, bg = "transparent")

# -------------------- EXPORTACIÓN DE TABLAS --------------------
message("> Exportando tablas resumen...")

exportar_csv <- function(df, columnas, nombre) {
  df %>% 
    st_drop_geometry() %>% 
    select(all_of(columnas), riqueza) %>% 
    arrange(desc(riqueza)) %>% 
    write.csv(paste0("./Outputs/", nombre), row.names = FALSE)
}

exportar_csv(riqueza_grid, c("utm10", "nuts1"), "Tabla_Riqueza_Grid.csv")
exportar_csv(riqueza_dist, c("Distrito", "nuts1"), "Tabla_Riqueza_Distritos.csv")
exportar_csv(riqueza_muni, c("Municipio", "nuts1"), "Tabla_Riqueza_Municipios.csv")

message("✔ Proceso finalizado. Archivos disponibles en la carpeta /Outputs")
