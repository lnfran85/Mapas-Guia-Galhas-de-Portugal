library(httr)
library(jsonlite)
library(dplyr)

project_id <- "galhas-de-portugal"

page <- 1
all_users <- c()

repeat {
  
  url <- paste0(
    "https://api.inaturalist.org/v1/projects/",
    project_id,
    "/members"
  )
  
  res <- GET(url, query = list(page = page, per_page = 200))
  data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  
  results <- data$results
  
  # si no hay más resultados, salimos
  if (length(results) == 0) break
  
  # extraer usernames
  users <- results$user$login
  
  all_users <- c(all_users, users)
  
  page <- page + 1
}

# quitar duplicados
all_users <- unique(all_users)

# convertir a data.frame
df <- data.frame(username = all_users)

# guardar CSV
write.csv(df, "miembros_inaturalist.csv", row.names = FALSE)

print(paste("Usuarios descargados:", length(all_users)))