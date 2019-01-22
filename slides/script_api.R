library(httr)
library(sf)
library(mapview)
library(tmap)

url <- "https://api-adresse.data.gouv.fr/search/"

query <- GET(url, query = list(q = "13 rue de l'Université, Paris"))
status_code(query)
geojson <- content(query, as = "text")
adresses <- read_sf(geojson)
mapview(adresses)

