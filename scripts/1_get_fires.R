library(tidyverse)
library(sf)
library(lubridate)
library(terra)

# Get CA boundary
ca <- maps::map('state', plot=F, fill=T) %>%
  st_as_sf() %>%
  filter(ID=='california') %>%
  st_transform(crs=4269) %>%
  vect()

# Set Paths
mtbs_shp <- 'data/raw/fire/mtbs_perimeter/mtbs_perims_DD.shp'

# Read and process the data
ca_recent_fires <- st_read(mtbs_shp) %>%
  filter(year(Ig_Date)>=2019) %>%
  vect() %>%
  crop(ca) %>%
  mask(ca) %>%
  st_as_sf()

# Filter the data
filtered <- ca_recent_fires %>%
  filter(BurnBndAc > 100000)
plot(filtered$geometry)

st_write(filtered, 'data/temp/ca_recent_fires.shp', delete_dsn = TRUE)
