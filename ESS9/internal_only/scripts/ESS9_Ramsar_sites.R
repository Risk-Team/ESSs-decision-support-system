### ESS9 data analysis - Ramsar sites

# Dependencies
library(raster)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(terra)
library(plotly)
library(dplyr)
library(leaflet)


# Load the data
ramsar_shp_incorrect <- read_sf("Ramsar_polygons/features_publishedPolygon.shp")

# Filter out invalid rows
valid <- st_is_valid(ramsar_shp_incorrect)
shapefile2 <- ramsar_shp_incorrect[valid, ]

# Remove boundaries between multipolygons
shapefile4 <- shapefile2 %>% 
  group_by(officialna, country_en, area_off) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# Convert from multipolygons to polygons
shapefile7 <- st_cast(shapefile4, "POLYGON")

# Make polygons valid
ramsar_shp_read_poly <- ramsar_shp_read_poly %>% st_make_valid()

# Correct format

worldmap <- rnaturalearth::ne_countries(returnclass = "sf")
newcrs <- crs(worldmap)

ramsar_shp_read_poly <- st_transform(shapefile7, newcrs)
ramsar_shp_read_poly <- ramsar_shp_read_poly[, c("officialna","country_en","area_off","geometry")]
colnames(ramsar_shp_read_poly) <- c("Park_name","Country","Size","geometry")
ramsar_shp_read_poly$dataset <- "Ramsar sites"

st_write(ramsar_shp_read_poly, "Ramsar_polygons/Ramsar_valid_singlepoly.shp")


# Plot shapefile
ramsar_sf <- st_read("Ramsar_polygons/Ramsar_valid_singlepoly.shp")

subsetdata <- subset(worldmap, continent == "South America")
new <- sf::st_crop(ramsar_sf, ext(subsetdata))

leaflet() %>% addTiles() %>% 
  addPolygons(data=new,weight=2,fillColor = "#E31A1C", col = "#E31A1C",highlightOptions = highlightOptions(color='white',weight=1,
                                                                                                           bringToFront = TRUE), label = new$Park_name) %>%
  addProviderTiles("Esri.WorldTopoMap")





