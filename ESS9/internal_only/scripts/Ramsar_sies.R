library(sf)
library(ggplot2)
library(tidyverse)
library(rnaturalearth)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# processing to make file valid

shapefilePath2 <-  "../internal_only/ramsar_site/features_publishedPolygon.shp"

shapefile2 <- st_read(shapefilePath2)

valid <- st_is_valid(shapefile2)

# Filter out invalid rows
shapefile2 <- shapefile2[valid, ]
# unite geometries
shapefile3 <- st_union(shapefile2) %>% 
  st_make_valid() %>% 
  st_cast(., "POLYGON")

st_write(shapefile3, "../9.1-9.4/ramsar_sites/ramsar.shp")


# plotting ----------------------------------------------------------------

IPL <- read_sf("../../ESS8/internal_only/IPL/IPL_2017.shp")

shp <- read_sf("../9.1-9.4/ramsar_sites/ramsar.shp") %>% 
  st_transform(., st_crs(IPL))

world <- ne_coastline(scale="medium",returnclass = "sf") %>%
  st_transform(., st_crs(IPL))

# for visualization and saving image

p <- ggplot()+
  geom_sf(data=world, fill = "white", color = "black", linewidth=0.1)+
  geom_sf(data=shp, fill="blue", color="blue", linewidth=0.1)+
  coord_sf(crs = st_crs(IPL)) +
  theme_bw()

ggsave(p, filename = "../9.1-9.4/plots/ramsar.png")

