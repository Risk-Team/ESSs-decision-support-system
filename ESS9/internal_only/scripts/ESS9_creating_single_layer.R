## ESS9 - Merging shapefiles into single layer

library(raster)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(terra)
library(plotly)
library(dplyr)
library(leaflet)
library(htmltools)
library(htmlwidgets)


## GIAHS

GIAHS_shp_read <- st_read("GIAHS_files/GIAHS.shp")
GIAHS_shp_read$dataset <- "GIAHS"

## UNESCO

UNESCO_shp_read <- st_read("UNESCO_files/UNESCO.shp")
UNESCO_shp_read$dataset <- "UNESCO"

## GeoParks

geopark_shp_read <- st_read("Geoparks_files/Geoparks.shp")
geopark_shp_read <- within(geopark_shp_read, rm(Website))
geopark_shp_read$dataset <- "GeoParks"

## National Parks

Nat_park_read <- st_read("National_park/National_park.shp")


## Ramsar sites
ramsar_shp_read <- st_read("Ramsar_shp/Ramsar_valid_singlepoly.shp")
ramsar_shp_read$dataset <- "Ramsar site"

# Make sure all have correct CRS

worldmap <- rnaturalearth::ne_countries(returnclass = "sf")
newcrs <- crs(worldmap)

ramsar_shp_read <- st_transform(ramsar_shp_read, newcrs)
geopark_shp_read <- st_transform(geopark_shp_read, newcrs)
GIAHS_shp_read <- st_transform(GIAHS_shp_read, newcrs)
UNESCO_shp_read <- st_transform(UNESCO_shp_read, newcrs)
Nat_park_read <- st_transform(Nat_park_read, newcrs)


# Convert Point shapefiles to polygons with a buffer

ESS9_layer_point <- rbind(geopark_shp_read, GIAHS_shp_read,UNESCO_shp_read)
ESS9_layer_pointcat <- st_buffer(ESS9_layer_point, dist=1500)
ESS9_layer_pointcat <- st_transform(ESS9_layer_pointcat, newcrs)

# Combine point/polygon files with other multipolygon shapefiles + convert to polygon
ESS9_layer_point_full <- rbind(ESS9_layer_pointcat, ramsar_shp_read, Nat_park_read)
ESS9_layer_point_full2 <- st_cast(ESS9_layer_point_full, "POLYGON")
ESS9_layer_point_full2 <- st_make_valid(ESS9_layer_point_full2)


# Write to new shapefile
st_write(ESS9_layer_point_full2, "ESS9_multilayer.shp")



## Plot shapefile
ESS9_shp_read <- st_read("ESS9_multilayer.shp")

subsetdata <- subset(worldmap, continent == "Africa")
new <- sf::st_crop(ESS9_shp_read, ext(subsetdata))

# Colour plot by dataset
qpal <- colorFactor(
  palette = c("#1F78B4", "black", "green", "#E31A1C", "#D95F02"),
  domain = new$dataset
)

palette = c("#1F78B4", "black", "#D95F02", "#E31A1C", "green")

leaflet() %>% addTiles() %>% 
  addPolygons(data=new,weight=2,fillColor = ~qpal(dataset), col = ~qpal(dataset),highlightOptions = highlightOptions(color='white',weight=1,
                                                                                                                       bringToFront = TRUE), label = new$Park_name) %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  addLegend(colors = palette, labels = unique(ESS9_shp_read$dataset))
  

leaflet() %>% addTiles() %>% 
  addPolygons(data=subsetnew,weight=2,fillColor = "#E31A1C", col = "#E31A1C",highlightOptions = highlightOptions(color='white',weight=1,
                                                                                                                     bringToFront = TRUE), label = subsetnew$Park_name) %>%
  addProviderTiles("Esri.WorldTopoMap")
