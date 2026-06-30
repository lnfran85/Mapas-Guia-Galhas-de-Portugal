library(magick)
library(stringr)
library(dplyr)

# 1. ENRUTAMIENTO REAL
dir_base       <- "Outputs/"
dir_mapas      <- "Outputs/"                             
dir_feno       <- "Outputs/Fenologia/"                   
dir_perfiles   <- "Outputs/Distribuicao_por_especie/"    

dir_salida     <- "Outputs/Laminas_Guia_Campo/"
if(!dir.exists(dir_salida)) dir.create(dir_salida, recursive = TRUE)

# Color antiguo que queremos "tapar" o forzar a blanco
color_antiguo <- "#F7F5F0" 

# 2. DETECTAR ESPECIES
archivos_mapas <- list.files(dir_mapas, pattern = "^1_Distritos_.*\\.png$")

especies_safe <- archivos_mapas %>% 
  str_replace("1_Distritos_", "") %>% 
  str_replace("\\.png$", "")

message("> Detectadas ", length(especies_safe), " especies para maquetar con fondo blanco unificado.")

# 3. BUCLE DE MAQUETACIÓN EN FONDO BLANCO
for (sp_safe in especies_safe) {
  message("--- Procesando láminas limpias para: ", sp_safe)
  
  ruta_mapa_dist <- file.path(dir_mapas, paste0("1_Distritos_", sp_safe, ".png"))
  ruta_mapa_muni <- file.path(dir_mapas, paste0("2_Municipios_", sp_safe, ".png"))
  
  ruta_feno <- file.path(dir_feno,     paste0(sp_safe, "_fenologia_circular.png"))
  ruta_alt  <- file.path(dir_perfiles, paste0(sp_safe, "_altitude.png"))
  ruta_mar  <- file.path(dir_perfiles, paste0(sp_safe, "_distancia_mar.png"))
  
  if (!file.exists(ruta_feno) | !file.exists(ruta_alt) | !file.exists(ruta_mar)) {
    next
  }
  
  # Paso 1: Cargar gráficos de datos
  img_feno <- image_read(ruta_feno)
  img_alt  <- image_read(ruta_alt)
  img_mar  <- image_read(ruta_mar)
  
  ancho_derecha <- 800  
  img_feno <- image_scale(img_feno, as.character(ancho_derecha))
  img_alt  <- image_scale(img_alt,  as.character(ancho_derecha))
  img_mar  <- image_scale(img_mar,  as.character(ancho_derecha))
  
  # Paso 2: Crear el bloque vertical derecho apilado
  columna_derecha <- image_append(c(img_feno, img_alt, img_mar), stack = TRUE)
  
  alto_total_datos  <- image_info(columna_derecha)$height
  ancho_total_datos <- image_info(columna_derecha)$width
  
  # GEOMETRÍA DE SOLAPAMIENTO Y RECORTES
  desplazamiento_x <- 800 
  recorte_izquierdo <- 800 
  
  # --- COMBINAR LÁMINA 1: DISTRITOS ---
  if (file.exists(ruta_mapa_dist)) {
    img_mapa_dist <- image_read(ruta_mapa_dist) %>% image_scale(paste0("x", alto_total_datos))
    
    ancho_mapa <- image_info(img_mapa_dist)$width
    ancho_mapa_recortado <- ancho_mapa - recorte_izquierdo
    
    img_mapa_dist <- image_crop(img_mapa_dist, paste0(ancho_mapa_recortado, "x", alto_total_datos, "+", recorte_izquierdo, "+0"))
    ancho_lienzo  <- ancho_mapa_recortado + ancho_total_datos - desplazamiento_x
    
    # TRUCO: Creamos el lienzo base en BLANCO PURO para evitar el efecto negro del visor
    #lamina_distritos <- image_blank(ancho_lienzo, alto_total_datos, color = "#FFFFFF") %>%
    lamina_distritos <- image_blank(ancho_lienzo, alto_total_datos, color = "#F7F5F0") %>%
      image_composite(img_mapa_dist, offset = "+0+0") %>%
      image_composite(columna_derecha, offset = paste0("+", ancho_mapa_recortado - desplazamiento_x, "+0"))
    
    image_write(lamina_distritos, 
                path = file.path(dir_salida, paste0("Ficha_Campo_Distritos_", sp_safe, ".png")), 
                format = "png")
    message("   ✔ Guardada lámina de distritos en fondo blanco.")
  }
  
  # --- COMBINAR LÁMINA 2: MUNICIPIOS ---
  if (file.exists(ruta_mapa_muni)) {
    img_mapa_muni <- image_read(ruta_mapa_muni) %>% image_scale(paste0("x", alto_total_datos))
    
    ancho_mapa <- image_info(img_mapa_muni)$width
    ancho_mapa_recortado <- ancho_mapa - recorte_izquierdo
    
    img_mapa_muni <- image_crop(img_mapa_muni, paste0(ancho_mapa_recortado, "x", alto_total_datos, "+", recorte_izquierdo, "+0"))
    ancho_lienzo  <- ancho_mapa_recortado + ancho_total_datos - desplazamiento_x
    
    # Composición sobre lienzo blanco
    #lamina_municipios <- image_blank(ancho_lienzo, alto_total_datos, color = "#FFFFFF") %>%
    lamina_municipios <- image_blank(ancho_lienzo, alto_total_datos, color = "#F7F5F0") %>%
      image_composite(img_mapa_muni, offset = "+0+0") %>%
      image_composite(columna_derecha, offset = paste0("+", ancho_mapa_recortado - desplazamiento_x, "+0"))
    
    image_write(lamina_municipios, 
                path = file.path(dir_salida, paste0("Ficha_Campo_Municipios_", sp_safe, ".png")), 
                format = "png")
  }
}

message("\n✔ ¡Solucionado! Ahora todas las láminas finales tienen fondo blanco plano y se verán perfectas en cualquier visor.")