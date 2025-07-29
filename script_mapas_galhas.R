# --- PACKAGES ---
# install.packages("sf")
# install.packages("ggplot2")
# install.packages("dplyr")

library(sf)
library(ggplot2)
library(dplyr)

# --- DIRECTORY SETUP ---
setwd("I:\\O meu disco\\")

if (!dir.exists("I:\\O meu disco\\Mapas_galhas_web")) {
  dir.create("I:\\O meu disco\\Mapas_galhas_web")
}
if (!dir.exists("I:\\O meu disco\\shapefiles")) {
  dir.create("I:\\O meu disco\\shapefiles")
}


# --- LOAD DATA ---
# Load your geographical data and species occurrence data.
distritos <- st_read("I:/O meu disco/CAOP_2024_distritos/Distritos_dissolvidos.shp") %>%
  st_make_valid()

#galhas_inat <- get_inat_obs_project(88083,type="observations") 
galhas_inat <- read.csv("C:\\Users\\LENOVO\\Downloads\\observations-599899.csv\\observations-599899.csv")
galhas_inat <- galhas_inat %>%
  mutate(
    latitude = ifelse(!is.na(private_latitude) & private_latitude != "", private_latitude, latitude),
    longitude = ifelse(!is.na(private_longitude) & private_longitude != "", private_longitude, longitude)
  )
##galhas_biblio <- read_excel("galhas_biblio.xlsx")
galhas_biblio <- galhas_inat

# --- ISLAND DEFINITIONS ---
# These lists help in handling the specific coordinates for the Azores and Madeira.
ilhas_acores <- c("Ilha da Graciosa", "Ilha das Flores", "Ilha de Santa Maria", "Ilha de São Jorge",
                  "Ilha de São Miguel", "Ilha do Corvo", "Ilha do Faial", "Ilha do Pico", "Ilha Terceira")
ilhas_madeira <- c("Ilha da Madeira", "Ilha de Porto Santo")

# --- DISPLACEMENT AND SCALING FUNCTION ---
# This function is used to reposition the islands for better visualization on the map.
deslocar_escalar <- function(sf_obj, deslocamento, centro, escala) {
  sf_obj %>%
    mutate(geometry = (st_geometry(.) + deslocamento - centro) * escala + centro) %>%
    st_set_crs(st_crs(distritos))
}

# --- GEOGRAPHICAL DATA PROCESSING ---
# Prepare the district data for the mainland, Madeira, and the Azores,
# applying specific transformations for each region to fit them onto one map.

# Madeira: Filter, cast to polygon, filter out small elements, and displace.
distritos_madeira <- distritos %>%
  filter(Distrito %in% ilhas_madeira) %>%
  st_cast("POLYGON") %>%
  filter(st_coordinates(st_centroid(.))[,2] > 32.5) %>%
  mutate(geometry = st_geometry(.) + c(5, 6)) %>%
  st_set_crs(st_crs(distritos))

# Continent: Filter out the islands.
distritos_cont <- distritos %>%
  filter(!Distrito %in% c(ilhas_acores, ilhas_madeira))

# Azores: Displace and scale the islands.
distritos_acores_deslocados <- distritos %>%
  filter(Distrito %in% ilhas_acores) %>%
  mutate(geometry = st_geometry(.) + c(16.5, 2.8))
centro_acores <- st_centroid(st_union(distritos_acores_deslocados))
fator_escala <- 0.5714
distritos_acores <- deslocar_escalar(distritos_acores_deslocados, c(0, 0), centro_acores, fator_escala)

# Combine all repositioned geographical data.
distritos_reposicionados <- bind_rows(distritos_cont, distritos_acores, distritos_madeira)

# Create a visual separator line for the map.
linha_separadora <- st_sfc(st_linestring(rbind(
  c(-13.3, 41.0),
  c(-10.5, 39.5)
)), crs = 4326) %>%
  st_transform(crs = st_crs(distritos)) %>%
  st_sf()

# --- SPECIES MAP AND SHAPEFILE GENERATION LOOP ---

# Get a unique list of all species from both data sources.
species_inat <- unique(galhas_inat$scientific_name)
species_biblio <- unique(galhas_biblio$scientific_name)
all_species <- unique(c(species_inat, species_biblio))

# Start the loop for each species.
system.time({
  for (species_name in all_species) {
    
    message(paste("Criando o mapa e a shapefile para a espécie:", species_name))
    
    # Filter occurrence data for the current species from both sources.
    coordenadas_inat_species <- galhas_inat %>% filter(scientific_name == species_name)
    coordenadas_biblio_species <- galhas_biblio %>% filter(scientific_name == species_name)
    
    # Combine the occurrence data.
    coordenadas_species <- bind_rows(
      coordenadas_inat_species %>% select(latitude, longitude),
      coordenadas_biblio_species %>% select(latitude, longitude)
    )
    
    # Convert the combined coordinates into an sf (simple features) object.
    coordenadas_sf <- st_as_sf(coordenadas_species, coords = c("longitude", "latitude"), crs = 4326)
    
    # Associate each point with its respective district (before any displacement).
    coordenadas_sf$Distrito <- apply(st_intersects(coordenadas_sf, distritos, sparse = FALSE), 1,
                                     function(row) {
                                       if (any(row)) distritos$Distrito[which(row)[1]] else NA
                                     })
    
    # Reposition the occurrence points according to their geographical region.
    coordenadas_acores <- coordenadas_sf %>%
      filter(Distrito %in% ilhas_acores) %>%
      mutate(geometry = st_geometry(.) + c(16.5, 2.8)) %>%
      deslocar_escalar(c(0, 0), centro_acores, fator_escala)
    
    coordenadas_madeira <- coordenadas_sf %>%
      filter(Distrito %in% ilhas_madeira) %>%
      mutate(geometry = st_geometry(.) + c(5, 6)) %>%
      st_set_crs(st_crs(coordenadas_sf))
    
    coordenadas_cont <- coordenadas_sf %>%
      filter(!(Distrito %in% c(ilhas_acores, ilhas_madeira)) & !is.na(Distrito))
    
    # Combine all repositioned points.
    coordenadas_corrigidas <- bind_rows(coordenadas_cont, coordenadas_acores, coordenadas_madeira) %>%
      st_transform(crs = st_crs(distritos_reposicionados))
    
    # Determine presence/absence for each district based on the corrected points.
    presenca_distritos <- st_intersects(distritos_reposicionados, coordenadas_corrigidas)
    distritos_reposicionados$presenca <- lengths(presenca_distritos) > 0
    
    # --- MAP CREATION ---
    # Generate the map for the current species.
    mapa_final <- ggplot() +
      geom_sf(data = distritos_reposicionados, aes(fill = presenca), color = "grey20") +
      scale_fill_manual(
        values = c("TRUE" = "darkkhaki", "FALSE" = "lightgrey"),
        labels = c("TRUE" = "Com presença", "FALSE" = "Sem presença")
      ) +
      geom_sf(data = linha_separadora, color = "grey50", linewidth = 0.3) +
      #labs(title = paste("Distribution of", species_name)) +
      labs(title = bquote(paste("Distribuição de ", italic(.(species_name))))) +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size = 16),
        axis.title = element_text(size=16),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      ) #+
      #annotation_scale(location = "br", bar_cols = c("grey60", "white"), text_cex = 2.0) +
      #annotation_north_arrow(location = "br", which_north = "true",
      #                       pad_x = unit(0.15, "in"), pad_y = unit(0.4, "in"),
      #                       style = north_arrow_minimal(fill = "grey40", line_col = "grey20", text_size = 14))
    
    # Save the map as a PNG file.
    ggsave(filename = paste0("Mapas_galhas_web/Distritos_", gsub(" ","_", species_name), ".png"), plot = mapa_final,  #mudar aqui por municipio se for o caso
           width = 1400, height = 1400, units = "px", dpi = 96)
    
    # --- SHAPEFILE EXPORT ---
    # Save the corrected points as a shapefile.
    #if (nrow(coordenadas_corrigidas) > 0) {
    #  st_write(coordenadas_corrigidas, paste0("shapefiles/Distritos_", gsub(" ", "_", species_name), "_points.shp"), delete_layer = TRUE)
    #} else {
    #  message(paste("No corrected points to save for species:", species_name))
    #}
  }
message("Todos os mapas e shapefiles para distritos criados!")
}) /60 # time in minutes loop







### ----- MUNICIPIOS -----


# --- LOAD DATA ---
# Load your geographical data and species occurrence data.
distritos <- st_read("I:/O meu disco/CAOP_2024_distritos/Municipios_dissolvidos.shp") %>%
  st_make_valid()

#galhas_inat <- get_inat_obs_project(88083,type="observations") 
galhas_inat <- read.csv("C:\\Users\\LENOVO\\Downloads\\observations-599899.csv\\observations-599899.csv")
galhas_inat <- galhas_inat %>%
  mutate(
    latitude = ifelse(!is.na(private_latitude) & private_latitude != "", private_latitude, latitude),
    longitude = ifelse(!is.na(private_longitude) & private_longitude != "", private_longitude, longitude)
  )
##galhas_biblio <- read_excel("galhas_biblio.xlsx")
galhas_biblio <- galhas_inat

# --- ISLAND DEFINITIONS ---
# These lists help in handling the specific coordinates for the Azores and Madeira.
ilhas_acores <- c("Vila do Porto", "Lagoa_Acores", "Nordeste", "Ponta Delgada", "Povoação",
                  "Ribeira Grande", "Vila Franca do Campo", "Angra do Heroísmo", "Praia da Vitória", "Santa Cruz da Graciosa",
                  "Calheta de São Jorge", "Velas", "Lajes do Pico", "Madalena", "São Roque do Pico","Horta", 
                  "Lajes das Flores", 
                  "Santa Cruz das Flores", 
                  "Corvo")

ilhas_madeira <- c("Calheta", "Câmara de Lobos", "Funchal", "Machico", "Ponta do Sol",
                   "Porto Moniz", "Ribeira Brava", "Santa Cruz", "Santana", "São Vicente", "Porto Santo")

# --- DISPLACEMENT AND SCALING FUNCTION ---
# This function is used to reposition the islands for better visualization on the map.
deslocar_escalar <- function(sf_obj, deslocamento, centro, escala) {
  sf_obj %>%
    mutate(geometry = (st_geometry(.) + deslocamento - centro) * escala + centro) %>%
    st_set_crs(st_crs(distritos))
}

# --- GEOGRAPHICAL DATA PROCESSING ---
# Prepare the district data for the mainland, Madeira, and the Azores,
# applying specific transformations for each region to fit them onto one map.

# Madeira: Filter, cast to polygon, filter out small elements, and displace.
distritos_madeira <- distritos %>%
  filter(Municipio %in% ilhas_madeira) %>%
  st_cast("POLYGON") %>%
  filter(st_coordinates(st_centroid(.))[,2] > 32.5) %>%
  mutate(geometry = st_geometry(.) + c(5, 6)) %>%
  st_set_crs(st_crs(distritos))

# Continent: Filter out the islands.
distritos_cont <- distritos %>%
  filter(!Municipio %in% c(ilhas_acores, ilhas_madeira))



# Azores: Displace and scale the islands.
distritos_acores_deslocados <- distritos %>%
  filter(Municipio %in% ilhas_acores) %>%
  mutate(geometry = st_geometry(.) + c(16.5, 2.8))


centro_acores <- st_centroid(st_union(distritos_acores_deslocados))
fator_escala <- 0.5714
distritos_acores <- deslocar_escalar(distritos_acores_deslocados, c(0, 0), centro_acores, fator_escala)

# Combine all repositioned geographical data.
distritos_reposicionados <- bind_rows(distritos_cont, distritos_acores, distritos_madeira)

# Create a visual separator line for the map.
linha_separadora <- st_sfc(st_linestring(rbind(
  c(-13.3, 41.0),
  c(-10.5, 39.5)
)), crs = 4326) %>%
  st_transform(crs = st_crs(distritos)) %>%
  st_sf()

# --- SPECIES MAP AND SHAPEFILE GENERATION LOOP ---

# Get a unique list of all species from both data sources.
species_inat <- unique(galhas_inat$scientific_name)
species_biblio <- unique(galhas_biblio$scientific_name)
all_species <- unique(c(species_inat, species_biblio))

# Start the loop for each species.
system.time({
  for (species_name in all_species) {
    
    message(paste("Criando o mapa e a shapefile para a espécie:", species_name))
    
    # Filter occurrence data for the current species from both sources.
    coordenadas_inat_species <- galhas_inat %>% filter(scientific_name == species_name)
    coordenadas_biblio_species <- galhas_biblio %>% filter(scientific_name == species_name)
    
    # Combine the occurrence data.
    coordenadas_species <- bind_rows(
      coordenadas_inat_species %>% select(latitude, longitude),
      coordenadas_biblio_species %>% select(latitude, longitude)
    )
    
    # Convert the combined coordinates into an sf (simple features) object.
    coordenadas_sf <- st_as_sf(coordenadas_species, coords = c("longitude", "latitude"), crs = 4326)
    
    # Associate each point with its respective district (before any displacement).
    coordenadas_sf$Municipio <- apply(st_intersects(coordenadas_sf, distritos, sparse = FALSE), 1,
                                     function(row) {
                                       if (any(row)) distritos$Municipio[which(row)[1]] else NA
                                     })
    
    # Reposition the occurrence points according to their geographical region.
    coordenadas_acores <- coordenadas_sf %>%
      filter(Municipio %in% ilhas_acores) %>%
     mutate(geometry = st_geometry(.) + c(16.5, 2.8)) %>%
      deslocar_escalar(c(0, 0), centro_acores, fator_escala)
    
    coordenadas_madeira <- coordenadas_sf %>%
      filter(Municipio %in% ilhas_madeira) %>%
      mutate(geometry = st_geometry(.) + c(5, 6)) %>%
      st_set_crs(st_crs(coordenadas_sf))
    
    coordenadas_cont <- coordenadas_sf %>%
      filter(!(Municipio %in% c(ilhas_acores, ilhas_madeira)) & !is.na(Municipio))

    
    # Combine all repositioned points.
    coordenadas_corrigidas <- bind_rows(coordenadas_cont, coordenadas_acores, coordenadas_madeira) %>%
      st_transform(crs = st_crs(distritos_reposicionados))

    
    # Determine presence/absence for each district based on the corrected points.
    presenca_distritos <- st_intersects(distritos_reposicionados, coordenadas_corrigidas)
    distritos_reposicionados$presenca <- lengths(presenca_distritos) > 0
    
    # --- MAP CREATION ---
    # Generate the map for the current species.
    mapa_final <- ggplot() +
      geom_sf(data = distritos_reposicionados, aes(fill = presenca), color = "grey20") +
      scale_fill_manual(
        values = c("TRUE" = "darkkhaki", "FALSE" = "lightgrey"),
        labels = c("TRUE" = "Com presença", "FALSE" = "Sem presença")
      ) +
      geom_sf(data = linha_separadora, color = "grey50", linewidth = 0.3) +
      #labs(title = paste("Distribution of", species_name)) +
      labs(title = bquote(paste("Distribuição de ", italic(.(species_name))))) +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size = 16),
        axis.title = element_text(size=16),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      ) #+
      #annotation_scale(location = "br", bar_cols = c("grey60", "white"), text_cex = 2.0) +
      #annotation_north_arrow(location = "br", which_north = "true",
      #                       pad_x = unit(0.15, "in"), pad_y = unit(0.4, "in"),
      #                       style = north_arrow_minimal(fill = "grey40", line_col = "grey20", text_size = 14))
    
    # Save the map as a PNG file.
    ggsave(filename = paste0("Mapas_galhas_web/Municipios_", gsub(" ","_", species_name), ".png"), plot = mapa_final,  #mudar aqui por municipio se for o caso
           width = 14, height = 8, units = "in", dpi = 300)
    
    # --- SHAPEFILE EXPORT ---
    # Save the corrected points as a shapefile.
    #if (nrow(coordenadas_corrigidas) > 0) {
    #  st_write(coordenadas_corrigidas, paste0("shapefiles/Municipios_", gsub(" ", "_", species_name), "_points.shp"), delete_layer = TRUE)
    #} else {
    #  message(paste("No corrected points to save for species:", species_name))
    #}
  }
  message("Todos os mapas e shapefiles para Municipios criados!")
}) 


write.csv2(levels(as.factor(all_species)),"lista_galhas.csv",row.names = F)
