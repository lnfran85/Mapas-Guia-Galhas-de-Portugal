# -------------------- PACKAGES --------------------
library(sf)
library(dplyr)
library(leaflet)
library(viridis)
library(htmlwidgets)

# -------------------- CONFIGURACIÓN DE RUTAS --------------------
path_csv        <- "./occur_data/observations-735195.csv/observations-735195.csv"
path_grid       <- "./Input_shp/utm10+ilhas_mod.gpkg"
path_distritos  <- "./Input_shp/CAOP_2024_distritos/Distritos_dissolvidos.shp"
path_muni       <- "./Input_shp/CAOP_2024_distritos/Municipios_dissolvidos.shp" 
crs_ref         <- 4326

# -------------------- CARGA Y PROCESADO OPTIMIZADO --------------------
message("> Cargando y simplificando capas base...")

# Simplificación para fluidez del HTML (tolerancia de ~50-100m)
distritos_base <- st_read(path_distritos, quiet = TRUE) %>% 
  st_make_valid() %>% st_transform(crs_ref) %>% st_simplify(dTolerance = 0.0005)

muni_base <- st_read(path_muni, quiet = TRUE) %>% 
  st_make_valid() %>% st_transform(crs_ref) %>% st_simplify(dTolerance = 0.0005)

grid_base <- st_read(path_grid, quiet = TRUE) %>% 
  st_make_valid() %>% st_transform(crs_ref)

message("> Procesando iNaturalist (Limpieza y alineación de CRS inmediata)...")
galhas_inat <- read.csv(path_csv) %>%
  mutate(lat = ifelse(!is.na(private_latitude) & private_latitude != "", private_latitude, latitude),
         lon = ifelse(!is.na(private_longitude) & private_longitude != "", private_longitude, longitude)) %>% 
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = crs_ref) %>%
  # Alinear CRS inmediatamente para ultra-fast intersection (Instrucción 2026-01-07)
  st_transform(st_crs(grid_base))

# -------------------- CÁLCULO DE RIQUEZA (MAPAS DE CALOR) --------------------
calc_riqueza <- function(capa, puntos) {
  idx <- st_intersects(capa, puntos)
  capa$riqueza <- sapply(idx, function(x) n_distinct(puntos$scientific_name[x], na.rm = TRUE))
  return(capa)
}

distritos_map <- calc_riqueza(distritos_base, galhas_inat)
muni_map      <- calc_riqueza(muni_base, galhas_inat)
grid_map      <- calc_riqueza(grid_base, galhas_inat)

lista_especies <- sort(unique(galhas_inat$scientific_name))

# -------------------- CONSTRUCCIÓN DEL VISOR LEAFLET --------------------
message("> Generando visor interactivo...")

pal_dist <- colorNumeric("viridis", domain = distritos_map$riqueza)
pal_muni <- colorNumeric("magma", domain = muni_map$riqueza)

mapa <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Mapa Claro") %>%
  
  # 1. CAPAS DE REFERENCIA (SOLO LÍMITES)
  addPolygons(data = distritos_base, group = "Límites: Distritos",
              fill = FALSE, color = "black", weight = 1.5, opacity = 0.5) %>%
  
  addPolygons(data = muni_base, group = "Límites: Municipios",
              fill = FALSE, color = "grey40", weight = 0.8, opacity = 0.4) %>%
  
  addPolygons(data = grid_base, group = "Límites: Grid UTM",
              fill = FALSE, color = "grey60", weight = 0.5, opacity = 0.3) %>%
  
  # 2. CAPAS DE RIQUEZA (HEATMAPS)
  addPolygons(data = distritos_map, group = "Riqueza: Distritos",
              fillColor = ~pal_dist(riqueza), fillOpacity = 0.5, weight = 1, color = "white",
              popup = ~paste0("<b>Distrito:</b> ", Distrito, "<br><b>Especies Únicas:</b> ", riqueza)) %>%
  
  addPolygons(data = muni_map, group = "Riqueza: Municipios",
              fillColor = ~pal_muni(riqueza), fillOpacity = 0.5, weight = 0.5, color = "white",
              popup = ~paste0("<b>Municipio:</b> ", Municipio, "<br><b>Especies Únicas:</b> ", riqueza)) %>%
  
  addPolygons(data = grid_map, group = "Riqueza: Grid UTM",
              fillColor = ~colorNumeric("plasma", grid_map$riqueza)(riqueza), 
              fillOpacity = 0.5, weight = 0.1, color = "grey",
              popup = ~paste0("<b>Cuadrícula:</b> ", utm10, "<br><b>Especies Únicas:</b> ", riqueza)) %>%

addPolygons(data = distritos_base, group = "carto", fillOpacity = 0.5, weight = 1, color = "white",
            popup = ~paste0("<b>Distrito:</b> ", Distrito)) %>%
addPolygons(data = muni_base, group = "carto", fillOpacity = 0.5, weight = 1, color = "white",
            popup = ~paste0("<b>Municipio:</b> ", Municipio)) %>%
addPolygons(data = grid_base, group = "carto", fillOpacity = 0.5, weight = 1, color = "white",
            popup = ~paste0("<b>grid UTM 10x10km:</b> ", utm10))

# 3. BUCLE PARA CADA ESPECIE (Puntos + Grid + Distritos)
for(sp in lista_especies) {
  p_sp   <- galhas_inat %>% filter(scientific_name == sp)
  
  # Intersecciones rápidas pre-calculadas para esta especie
  g_pres <- grid_base[as.logical(lengths(st_intersects(grid_base, p_sp))), ]
  d_pres <- distritos_base[as.logical(lengths(st_intersects(distritos_base, p_sp))), ]
  m_pres <- muni_base[as.logical(lengths(st_intersects(muni_base, p_sp))), ]
  
  mapa <- mapa %>%
    # Distritos donde está la especie
    addPolygons(data = d_pres, group = sp, fillOpacity = 0.1, weight = 2, color = "#2c7bb6", fill = TRUE) %>%
    # Municipios donde está la especie
    addPolygons(data = m_pres, group = sp, fillOpacity = 0.1, weight = 2, color = "#2c7bb6", fill = TRUE) %>%
    # Cuadrículas donde está la especie
    addPolygons(data = g_pres, group = sp, fillOpacity = 0.3, weight = 1, color = "darkgreen", fill = TRUE) %>%
    # Puntos exactos
    addCircleMarkers(data = p_sp, group = sp, radius = 3.5, color = "black", 
                     fillColor = "yellow", weight = 1, fillOpacity = 1,
                     popup = ~paste0("<b>", scientific_name, "</b><br>Fecha: ", observed_on))
}

# -------------------- CONTROLES Y LEYENDA --------------------
capas_limites <- c("Límites: Distritos", "Límites: Municipios", "Límites: Grid UTM")
capas_riqueza <- c("Riqueza: Distritos", "Riqueza: Municipios", "Riqueza: Grid UTM")

mapa <- mapa %>%
  addLayersControl(
    baseGroups = c("Mapa Claro", "OpenStreetMap"),
    overlayGroups = c(capas_limites, capas_riqueza, lista_especies),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  # Configuramos qué capas se ven al abrir el mapa (Límites de distritos y riqueza por distrito)
  hideGroup(c(capas_limites[-1], capas_riqueza[-1], lista_especies)) %>%
  addLegend(pal = pal_dist, values = distritos_map$riqueza, 
            title = "Riqueza Total (Dist.)", position = "bottomright")

# GUARDAR
if(!dir.exists("Outputs")) dir.create("Outputs")
saveWidget(mapa, file = "./Outputs/Visor_Final_Interactivo1.html", selfcontained = TRUE)

message("✔ Visor completado con éxito.")