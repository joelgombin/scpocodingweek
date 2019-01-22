library(sf)
toilets <- read_sf("./data/sanisettesparis2011.geojson")
toilets


churches <- read_sf("./data/churches.geojson")

library(tmap)
library(tmaptools)

basemap <- read_osm(churches, type = "stamen-toner")

tm_shape(basemap) +
  tm_rgb() +
tm_shape(churches) +
    tm_dots(shape = 3, col = "red") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass()
