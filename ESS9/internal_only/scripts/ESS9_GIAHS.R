### ESS9 data analysis - GIAHS


# Dependencies
require(measurements)
require(stringr)
require(dplyr)
require(terra)
require(sf)
require(raster)
library(leaflet)
library(htmltools)

# Load the data
GIAHS <- read.csv("GIAHS_data.csv",fileEncoding="UTF-8-BOM")

# Remove spaces from coordinates (due to copy/paste overshoots)

GIAHS$Longitude..center.point. <- str_trim(GIAHS$Longitude..center.point.)
GIAHS$Latitude..center.point. <- str_trim(GIAHS$Latitude..center.point.)

# GIAHS data is already in decimal format

GIAHS_coord_data <- cbind(GIAHS$Longitude..center.point., GIAHS$Latitude..center.point.)



## Make ShapeFile with GIAHS information

crdref <- "+proj=longlat +datum=WGS84"
GIAHS_vect <- vect(GIAHS_coord_data, crs=crdref)

class(GIAHS_vect) 
geom(GIAHS_vect)
ext(GIAHS_vect)

GIAHS_vect$Country <- GIAHS$Country
GIAHS_vect$Park_name <- GIAHS$System_name
GIAHS_vect$Size <- GIAHS$Size..Area.

GIAHS_sf <- st_as_sf(GIAHS_vect)
class(GIAHS_sf)
head(GIAHS_sf)

st_write(GIAHS_sf, "GIAHS.shp")


## Visualising GIAHS data

# Set up base-map

worldmap <- rnaturalearth::ne_countries(returnclass = "sf")
crs(worldmap)
ext(worldmap)


# Plot GIAHS shapefile with worldmap

GIAHS_shp_read <- st_read("GIAHS_files/GIAHS.shp")
ext(GIAHS_shp_read)
head(GIAHS_shp_read)

subsetdata <- subset(worldmap, continent == "South America")

ext(subsetdata)
new <- sf::st_crop(GIAHS_sf, ext(subsetdata))

leaflet() %>% addTiles() %>% 
  addPolygons(data=new,weight=2,fillColor = "#E31A1C", col = "#E31A1C",highlightOptions = highlightOptions(color='white',weight=1,
                                                                                                                 bringToFront = TRUE), label = new$Park_name) %>%
  addProviderTiles("Esri.WorldTopoMap")


