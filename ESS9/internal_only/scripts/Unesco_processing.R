library(sf)
library(tidyverse)
library(readxl)
library(rnaturalearth)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


p1 <- read_excel("/Users/ricca/Dropbox/RIKI/FAO/safeguards team/ESS app/data/ESS9/internal_only/UNESCO sites.xls") %>% 
  select(unique_number, name_en, area_hectares, latitude, longitude)


sf_df <- st_as_sf(p1, coords = c("longitude", "latitude"), crs = 4326)

st_write(sf_df, "/Users/ricca/Dropbox/RIKI/FAO/safeguards team/ESS app/data/ESS9/9.1-9.4/UNESCO_sites/Unesco.shp")


# plotting ----------------------------------------------------------------

IPL <- read_sf("../../ESS8/internal_only/IPL/IPL_2017.shp")

shp <- read_sf("../9.1-9.4/UNESCO_sites/Unesco.shp") %>% 
  st_transform(., st_crs(IPL))

world <- ne_coastline(scale="medium",returnclass = "sf") %>%
  st_transform(., st_crs(IPL))

p <- ggplot()+
  geom_sf(data=world, fill = "white", color = "black", linewidth=0.1)+
  geom_sf(data=shp, fill="blue", color="blue", size=0.4, alpha=0.5)+
  coord_sf(crs = st_crs(IPL)) +
  theme_bw()

ggsave(p, filename = "../9.1-9.4/plots/unesco.png")

