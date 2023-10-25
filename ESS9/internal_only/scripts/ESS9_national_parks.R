### ESS9 data analysis - National Parks

## Opening + splitting large shapefile

library(raster)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(terra)
library(plotly)
library(dplyr)
library(leaflet)
library(htmltools)
library("countrycode")

shp_1 <- st_read("WDPA_Oct2023_Public_shp_0/WDPA_Oct2023_Public_shp-polygons.shp")
shp_2 <- st_read("WDPA_Oct2023_Public_shp_1/WDPA_Oct2023_Public_shp-polygons.shp")
shp_3 <- st_read("WDPA_Oct2023_Public_shp_2/WDPA_Oct2023_Public_shp-polygons.shp")

shp_4 <- rbind(shp_1,shp_2,shp_3)

shp_5 <- shp_4[grepl("National",shp_4$DESIG_ENG),]
shp_6 <- shp_5[grepl("Park",shp_5$DESIG_ENG),]
shp_7 <- shp_5[grepl("park",shp_5$DESIG_ENG),]

shp_8 <- rbind(shp_6,shp_7)

shp_8$Country <- countrycode(shp_8$ISO3, origin = 'iso3c', destination='country.name')
Nat_park <- shp_8[, c("ORIG_NAME","REP_AREA","Country","geometry")]


# Filter out invalid rows
valid <- st_is_valid(Nat_park)
Nat_park2 <- Nat_park[valid, ]

# Remove buffer zones
Nat_park3 <- Nat_park2[!grepl( "[Aire D'Adhésion]", Nat_park2$ORIG_NAME, fixed = TRUE),]

# Convert to polygons
Nat_park4 <- Nat_park3 %>% 
  group_by(ORIG_NAME, REP_AREA, Country) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

Nat_park5 <- st_cast(Nat_park4, "POLYGON")


# Change CRS and add column labels

worldmap <- rnaturalearth::ne_countries(returnclass = "sf")
newcrs <- crs(worldmap)
Nat_park5_crs <- st_transform(Nat_park5, newcrs)

colnames(Nat_park5_crs) <- c("Park_name","Size","Country","geometry")
Nat_park5_crs$dataset <- "National park"

st_write(Nat_park5_crs, "National_park/National_park.shp")


# Plot National parks

subsetdata <- subset(worldmap, name_en == "France")

new <- sf::st_crop(Nat_park5_crs, ext(subsetdata))

leaflet() %>% addTiles() %>% 
  addPolygons(data=new,weight=2,fillColor = "green", col = "green",highlightOptions = highlightOptions(color='white',weight=1,
                                                                                                                     bringToFront = TRUE), label = new$Park_name) %>%
  addProviderTiles("Esri.WorldTopoMap")



