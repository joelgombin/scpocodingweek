library(sf)
toilets <- read_sf("./data/sanisettesparis2011.geojson")
toilets

arrondissements <- read_sf("./data/arrondissements.geojson")
churches <- read_sf("./data/churches.geojson", promote_to_multi = FALSE)
mosques <- read_sf("./data/mosque.geojson")

library(tmap)
library(tmaptools)

churches <- st_transform(churches, crs = 2154)
mosques <- st_transform(mosques, crs = 2154)
arrondissements <- st_transform(arrondissements, crs = 2154)

basemap <- read_osm(churches, type = "stamen-toner")

churches_density <- smooth_map(churches, bandwidth = 0.5, cover = arrondissements)
mosques_density <- smooth_map(mosques, bandwidth = 0.5, cover = arrondissements)

pdf("./carte.pdf", width = 10, height = 7)
tm_shape(arrondissements) +
  tm_borders() +
tm_shape(churches_density$polygons) +
  tm_fill(col = "level", palette = "Blues", title = "Churches density", alpha = 0.5) +
tm_shape(churches) +
  tm_dots(shape = 3, col = "blue") +
tm_shape(mosques_density$polygons) +
  tm_fill(col ="level", palette = "Greens", title = "Mosques density", alpha = 0.5) +
tm_shape(mosques) +
  tm_dots(shape = 8, col = "green") +
  tm_style("gray", title = "Some places of worship in Paris") +
  tm_scale_bar(position = c("left", "BOTTOM")) +
  tm_compass() +
  tm_legend(outside = TRUE) 
dev.off()
