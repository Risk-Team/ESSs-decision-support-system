### ESS9 data analysis - Geoparks


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
geopark <- read.csv("Geopark_data.csv",fileEncoding="UTF-8-BOM")



# Fix data errors - there are some data entry errors in the Geoparks website which are fixed below

# Chile Kütralkura UNESCO Global Geopark requires a Long-Lat change
chile_lat <- geopark$Longitude..[13]
chile_long <- geopark$Latitude..[13]
geopark$Longitude..[13] <- chile_long
geopark$Latitude..[13] <- chile_lat


# China's XINGWEN UNESCO GLOBAL GEOPARK and ZHANGJIAJIE UNESCO GLOBAL GEOPARK have E instead of N in latitude
geopark$Latitude..[46] <- sub('.', 'N', geopark$Latitude..[46])
geopark$Latitude..[51] <- sub('.', 'N', geopark$Latitude..[51])


# Finland's Lauhanvuori-Hämeenkangas UNESCO Global Geopark had W instead of E for longitude
geopark$Longitude..[62] <- "E22°10'30\""


# Germany's Thuringia Inselsberg -Drei Gleichen UNESCO Global Geopark has a typo in latitude
geopark$Latitude..[78] <- sub('550', '50', geopark$Latitude..[78])


# United Kingdom's GEOMÔN UNESCO GLOBAL GEOPARK has lat and long entered together
geopark$Longitude..[170] <- sub('N53°16’08”, W004°20’03”', 'W004°20’03”', geopark$Longitude..[170])
geopark$Latitude..[170] <- sub('N53°16’08”, W004°20’03”', 'N53°16’08”', geopark$Latitude..[170])



## Clean up latitude and longitudes - many contain ranges, special characters etc.

# Latitude clean

### DECISION - when coordinates have a range, only the first value in range was selected. ###

# Find symbols indicative of ranges and extract all coordinates before the symbol
tofind <- c("一","—","～","to",",","-","–","~")
removelist <- c(".*(?=\\一)", ".*(?=\\—)", ".*(?=\\～)", ".*(?=\\ to)", ".*(?=\\,)", ".*(?=\\-)", ".*(?=\\–)",".*(?=\\~)")

geopark$latitude_edit <- ifelse(grepl(paste(tofind, collapse = "|"),geopark$Latitude..), str_extract(geopark$Latitude.., paste(removelist, collapse = "|")), geopark$Latitude..)


# Remove North/south labels and convert "south" latitudes to negative values (in line with decimal values)
geopark$latitude_edit <- str_trim(geopark$latitude_edit)

geopark$latitude_edit <- ifelse(grepl("N", geopark$latitude_edit), ifelse(substr(geopark$latitude_edit, 1, 1) == "N", sub("^.", "", geopark$latitude_edit), geopark$latitude_edit), geopark$latitude_edit)
geopark$latitude_edit <- ifelse(grepl("S", geopark$latitude_edit), ifelse(substr(geopark$latitude_edit, 1, 1) == "S", sub("^.", "-", geopark$latitude_edit), geopark$latitude_edit), geopark$latitude_edit)

geopark$latitude_edit <- gsub('N','',geopark$latitude_edit)
geopark$latitude_edit <- gsub('S','',geopark$latitude_edit)


# Convert all degree, minutes, seconds values to spaces and decimals
toremove = c('°','\'','"','′','″','〞','’','”','ˊ','´')

geopark$latitude_edit <- gsub(paste(toremove, collapse = "|"),' ', geopark$latitude_edit)

geopark$latitude_edit_trim <- str_trim(geopark$latitude_edit)
geopark$latitude_edit_trim <- ifelse(str_count(geopark$latitude_edit_trim, " ") == 1, paste0(geopark$latitude_edit_trim, " 0"),geopark$latitude_edit_trim)
geopark$latitude_edit_trim = gsub('  ',' ', geopark$latitude_edit_trim)

geopark$latitude_edit_trim <- ifelse(str_count(geopark$latitude_edit_trim, " ") == 2, gsub('\\.','', geopark$latitude_edit_trim),geopark$latitude_edit_trim)
geopark$latitude_edit_trim = gsub(' ','.', geopark$latitude_edit_trim)
geopark$latitude_edit_trim = ifelse(str_count(geopark$latitude_edit_trim, "\\.") == 2, sub('\\.', ' ', geopark$latitude_edit_trim), geopark$latitude_edit_trim)



# Longitudes - same approach as latitudes and same decision made

# Find symbols indicative of ranges and extract all coordinates before the symbol
tofind <- c("一","—","～","to",",","-","–","~")
removelist <- c(".*(?=\\一)", ".*(?=\\—)", ".*(?=\\～)", ".*(?=\\ to)", ".*(?=\\,)", ".*(?=\\-)", ".*(?=\\–)",".*(?=\\~)")

geopark$longitude_edit <- ifelse(grepl(paste(tofind, collapse = "|"),geopark$Longitude..), str_extract(geopark$Longitude.., paste(removelist, collapse = "|")), geopark$Longitude..)


# Remove East/West labels and convert "West" latitudes to negative values (in line with decimal values)
geopark$longitude_edit <- str_trim(geopark$longitude_edit)

geopark$longitude_edit <- ifelse(grepl("E", geopark$longitude_edit), ifelse(substr(geopark$longitude_edit, 1, 1) == "E", sub("^.", "", geopark$longitude_edit), geopark$longitude_edit), geopark$longitude_edit)
geopark$longitude_edit <- ifelse(grepl("W", geopark$longitude_edit), ifelse(substr(geopark$longitude_edit, 1, 1) == "W", sub("^.", "-", geopark$longitude_edit), geopark$longitude_edit), geopark$longitude_edit)

geopark$longitude_edit <- gsub('E','',geopark$longitude_edit)
geopark$longitude_edit <- gsub('W','',geopark$longitude_edit)


# Convert all degree, minutes, seconds values to spaces and decimals
toremove = c('°','\'','"','′','″','〞','’','”','ˊ','´')

geopark$longitude_edit <- gsub(paste(toremove, collapse = "|"),' ', geopark$longitude_edit)

geopark$longitude_edit_trim <- str_trim(geopark$longitude_edit)
geopark$longitude_edit_trim <- ifelse(str_count(geopark$longitude_edit_trim, " ") == 1, paste0(geopark$longitude_edit_trim, " 0"),geopark$longitude_edit_trim)
geopark$longitude_edit_trim = gsub('  ',' ', geopark$longitude_edit_trim)

geopark$longitude_edit_trim <- ifelse(str_count(geopark$longitude_edit_trim, " ") == 2, gsub('\\.','', geopark$longitude_edit_trim),geopark$longitude_edit_trim)
geopark$longitude_edit_trim = gsub(' ','.', geopark$longitude_edit_trim)
geopark$longitude_edit_trim = ifelse(str_count(geopark$longitude_edit_trim, "\\.") == 2, sub('\\.', ' ', geopark$longitude_edit_trim), geopark$longitude_edit_trim)



## Convert latitude and longitude values to decimal coordinates

# Some coordinates are already in decimal form so extract coordinates with spaces (degree format)
geopark_longlat <- dplyr::filter(geopark, grepl(" ", geopark$latitude_edit_trim))

geopark_dec_already <- dplyr::filter(geopark, !grepl(" ", geopark$latitude_edit_trim))

geopark_longlat$latitude_dec <- measurements::conv_unit(geopark_longlat$latitude_edit_trim, from = 'deg_dec_min', to = 'dec_deg')
geopark_longlat$longitude_dec <- measurements::conv_unit(geopark_longlat$longitude_edit_trim, from = 'deg_dec_min', to = 'dec_deg')

geopark_dec_already$latitude_dec <- geopark_dec_already$latitude_edit_trim
geopark_dec_already$longitude_dec <- geopark_dec_already$longitude_edit_trim

geopark_with_dec_coord <- rbind(geopark_longlat,geopark_dec_already)
geopark_with_dec_coord$latitude_dec <- as.numeric(geopark_with_dec_coord$latitude_dec)
geopark_with_dec_coord$longitude_dec <- as.numeric(geopark_with_dec_coord$longitude_dec)


## Make ShapeFile with GeoPark information

geopark_coord_data <- cbind(geopark_with_dec_coord$longitude_dec, geopark_with_dec_coord$latitude_dec)

crdref <- "+proj=longlat +datum=WGS84"
geopark_vect <- vect(geopark_coord_data, crs=crdref)

class(geopark_vect) 
geom(geopark_vect)
ext(geopark_vect)

geopark_vect$Country <- geopark_with_dec_coord$Country..
geopark_vect$Park_name <- geopark_with_dec_coord$Geopark.Name..
geopark_vect$Size <- geopark_with_dec_coord$Size.
geopark_vect$Website <- geopark_with_dec_coord$WebSite..

geopark_sf <- st_as_sf(geopark_vect)
class(geopark_sf)
head(geopark_sf)

st_write(geopark_sf, "Geoparks.shp")


## Visualising GeoParks data

# Set up base-map

worldmap <- rnaturalearth::ne_countries(returnclass = "sf")
crs(worldmap)
ext(worldmap)


# Plot GeoParks shapefil with worldmap

geopark_shp_read <- st_read("Geoparks_shapefile/Geoparks.shp")
ext(geopark_shp_read)
head(geopark_shp_read)

leaflet() %>% addTiles() %>% 
  addPolygons(data=geopark_shp_read,weight=2,fillColor = "#E31A1C", col = "#E31A1C",highlightOptions = highlightOptions(color='white',weight=1,
                                                                                                           bringToFront = TRUE), label = geopark_shp_read$Park_name) %>%
  addProviderTiles("Esri.WorldTopoMap")

