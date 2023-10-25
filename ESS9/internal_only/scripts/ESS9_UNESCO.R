### ESS9 data analysis - UNESCO world heritage list


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
UNESCO <- read.csv("UNESCO_WHL_sites.csv",fileEncoding="UTF-8-BOM")


# UNESCO WHL data is very clean and already in accurate degree/minute/second format

# Remove North/south & East/West labels and convert "south" and "west" latitudes to negative values
UNESCO$Longitude_edit <- gsub('.*E','E',UNESCO$Longitude)
UNESCO$Longitude_edit <- gsub('.*W','W',UNESCO$Longitude_edit)

UNESCO$Longitude_edit <- gsub('E','',UNESCO$Longitude_edit)
UNESCO$Longitude_edit <- gsub('W','-',UNESCO$Longitude_edit)

UNESCO$Latitude_edit <- gsub('E.*','',UNESCO$Latitude)
UNESCO$Latitude_edit <- gsub('W.*','',UNESCO$Latitude_edit)

UNESCO$Latitude_edit <- gsub('N','',UNESCO$Latitude_edit)
UNESCO$Latitude_edit <- gsub('S','-',UNESCO$Latitude_edit)

UNESCO$Latitude_edit <- str_trim(UNESCO$Latitude_edit)



## Convert latitude and longitude values to decimal coordinates

UNESCO$Latitude_edit <- gsub(' ',',',UNESCO$Latitude_edit)
UNESCO$Latitude_edit <- gsub('\\.','',UNESCO$Latitude_edit)
UNESCO$Latitude_edit <- sub(',',' ',UNESCO$Latitude_edit)
UNESCO$Latitude_edit <- sub(',','\\.',UNESCO$Latitude_edit)

UNESCO$Longitude_edit <- gsub(' ',',',UNESCO$Longitude_edit)
UNESCO$Longitude_edit <- gsub('\\.','',UNESCO$Longitude_edit)
UNESCO$Longitude_edit <- sub(',',' ',UNESCO$Longitude_edit)
UNESCO$Longitude_edit <- sub(',','\\.',UNESCO$Longitude_edit)



UNESCO$latitude_dec <- measurements::conv_unit(UNESCO$Latitude_edit, from = 'deg_dec_min', to = 'dec_deg')
UNESCO$longitude_dec <- measurements::conv_unit(UNESCO$Longitude_edit, from = 'deg_dec_min', to = 'dec_deg')



## Make SpatVector and ShapeFile with UNESCO information

UNESCO_coord_data <- cbind(UNESCO$longitude_dec, UNESCO$latitude_dec)

crdref <- "+proj=longlat +datum=WGS84"
UNESCO_vect <- vect(UNESCO_coord_data, crs=crdref)

class(UNESCO_vect) 
geom(UNESCO_vect)
ext(UNESCO_vect)

UNESCO_vect$Country <- UNESCO$Country
UNESCO_vect$Park_name <- UNESCO$Place_name
UNESCO_vect$Size <- UNESCO$Surface

UNESCO_sf <- st_as_sf(UNESCO_vect)
class(UNESCO_sf)
head(UNESCO_sf)

st_write(UNESCO_sf, "UNESCO.shp")


## Visualising UNESCO data

# Set up base-map

worldmap <- rnaturalearth::ne_countries(returnclass = "sf")
crs(worldmap)
ext(worldmap)


# Plot UNESCO shapefile and worldmap

UNESCO_shp_read <- st_read("UNESCO_files/UNESCO.shp")
ext(geopark_shp_read)
head(geopark_shp_read)

subsetdata <- subset(worldmap, continent == "South America")

new <- sf::st_crop(UNESCO_sf, ext(subsetdata))

leaflet() %>% addTiles() %>% 
  addPolygons(data=new,weight=2,fillColor = "#E31A1C", col = "#E31A1C",highlightOptions = highlightOptions(color='white',weight=1,
                                                                                                                 bringToFront = TRUE), label = new$Park_name) %>%
  addProviderTiles("Esri.WorldTopoMap")
