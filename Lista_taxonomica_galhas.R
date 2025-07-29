# --- 1. Instalar y cargar el paquete taxize ---
library(taxize)

# --- 2. Cargar tu lista de especies desde un archivo CSV ---
# Asume que tu CSV tiene una columna con los nombres de las especies.

ruta_archivo_csv <- "I:\\O meu disco\\lista_galhas.csv" # ¡Cambia esto!
nombre_columna_especies <- "species" 

# Cargar el CSV
tryCatch({
  datos_especies <- read.csv(ruta_archivo_csv, header = TRUE, stringsAsFactors = FALSE)
}, error = function(e) {
  stop(paste("Error al cargar el archivo CSV:", e$message,
             "\nAsegúrate de que la ruta y el nombre del archivo son correctos, y de que el archivo existe."))
})

# Extraer la columna de nombres de especies
if (is.numeric(nombre_columna_especies)) {
  mis_especies <- datos_especies[, nombre_columna_especies]
} else {
  mis_especies <- datos_especies[[nombre_columna_especies]]
}

# Limpiar posibles valores NA o vacíos en la lista de especies
mis_especies <- mis_especies[!is.na(mis_especies) & mis_especies != ""]

# Si no hay especies después de cargar, detener el script
if (length(mis_especies) == 0) {
  stop("La columna de especies está vacía o no se encontraron nombres válidos en el archivo CSV.")
}

message(paste("Se han cargado", length(mis_especies), "especies desde el archivo CSV."))

# --- 3. Obtener la taxonomía completa ---
# Utilizaremos la función 'classification()' para consultar bases de datos en línea.
# 'gbif' (Global Biodiversity Information Facility) es una buena opción general.
# Puedes añadir otras bases de datos como 'ncbi', 'itis', 'worms' (para marinas)
# si 'gbif' no arroja resultados o para mayor robustez.
# La consulta puede tardar dependiendo de la cantidad de especies y tu conexión a internet.
message("\nConsultando bases de datos taxonómicas... esto puede tardar unos minutos.")
taxonomia_completa <- classification(mis_especies, db = 'gbif', rows = 1)

# 'rows = 1' le dice a 'taxize' que solo tome el primer resultado si hay múltiples coincidencias,
# lo cual es útil para la mayoría de los casos.

# --- 4. Procesar los resultados en un data.frame manejable (REVISADO) ---
# Inicializamos un data.frame vacío para almacenar la taxonomía organizada.
df_taxonomia <- data.frame(
  species = character(),
  kingdom = character(),
  phylum = character(),
  class = character(),
  order = character(),
  family = character(),
  genus = character(),
  stringsAsFactors = FALSE
)

# Iterar sobre cada resultado de la consulta para extraer los rangos.
for (i in seq_along(taxonomia_completa)) {
  species_name <- names(taxonomia_completa)[i] # Obtiene el nombre de la especie original
  
  # Verificar si se encontró información taxonómica Y si es un data.frame (o list)
  if (!is.null(taxonomia_completa[[i]]) && is.data.frame(taxonomia_completa[[i]])) {
    tax_info <- taxonomia_completa[[i]] # Accede al data.frame de taxonomía para la especie actual
    
    # Extraer los rangos deseados. Usamos 'ifelse' para poner NA si un rango no se encuentra.
    kingdom <- ifelse("kingdom" %in% tax_info$rank, tax_info$name[tax_info$rank == "kingdom"], NA_character_)
    phylum <- ifelse("phylum" %in% tax_info$rank, tax_info$name[tax_info$rank == "phylum"], NA_character_)
    class <- ifelse("class" %in% tax_info$rank, tax_info$name[tax_info$rank == "class"], NA_character_)
    order <- ifelse("order" %in% tax_info$rank, tax_info$name[tax_info$rank == "order"], NA_character_)
    family <- ifelse("family" %in% tax_info$rank, tax_info$name[tax_info$rank == "family"], NA_character_)
    genus <- ifelse("genus" %in% tax_info$rank, tax_info$name[tax_info$rank == "genus"], NA_character_)
    
    # Añadir esta fila al data.frame principal
    df_taxonomia <- rbind(df_taxonomia, data.frame(
      species = species_name,
      kingdom = kingdom,
      phylum = phylum,
      class = class,
      order = order,
      family = family,
      genus = genus,
      stringsAsFactors = FALSE
    ))
  } else {
    # Si no se encontró ninguna información válida para la especie (NULL o no-data.frame)
    message(paste("No se encontró información taxonómica válida para:", species_name))
    df_taxonomia <- rbind(df_taxonomia, data.frame(
      species = species_name,
      kingdom = NA_character_,
      phylum = NA_character_,
      class = NA_character_,
      order = NA_character_,
      family = NA_character_,
      genus = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
}

# --- 5. Mostrar los resultados ---
message("\nResultados de la taxonomía:")
print(df_taxonomia)

# --- 6. Opcional: Guardar los resultados ---
write.csv(df_taxonomia, "taxonomia_especies_completa.csv", row.names = FALSE)
message("\nResultados guardados en 'taxonomia_especies_completa.csv'")
