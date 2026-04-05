library(tidyverse)
library(sf)
library(terra)


## -------------------------------- Global Vars -------------------------------- ##
ca_boundary_folder <- 'data/raw/boundaries'
mtbs_shp <- 'data/raw/fire/mtbs_perimeter/mtbs_perims_DD.shp'
fire_out_folder <- 'data/input/fires'


## ----------------------------- Get CA Boundaries ----------------------------- ##
ca <- maps::map('state', plot=F, fill=T) %>%
  st_as_sf() %>%
  filter(ID=='california') %>%
  st_transform(crs=4269) %>%
  vect()
if (!file.exists(file.path(ca_boundary_folder, 'ca.shp'))) {
  write_sf(st_as_sf(ca), file.path(ca_boundary_folder, 'ca.shp'), delete_dsn = TRUE)
}

ca_counties <- maps::map('county', plot=F, fill=T) %>%
  st_as_sf() %>%
  mutate(state=substr(ID, 1, regexpr(',', ID)-1), county=substr(ID, regexpr(',', ID)+1, nchar(ID))) %>%
  filter(state=='california') %>%
  st_transform(crs=4269) %>%
  vect()
if (!file.exists(file.path(ca_boundary_folder, 'ca_counties.shp'))) {
  write_sf(st_as_sf(ca_counties), file.path(ca_boundary_folder, 'ca_counties.shp'), delete_dsn = TRUE)
}


## ------------------------------- Get 2020 Fires ------------------------------ ##
ca_2020_fires <- st_read(mtbs_shp) %>%
  filter(year(Ig_Date) == 2020) %>%
  vect() %>%
  crop(ca) %>%
  mask(ca) %>%
  st_as_sf()

ca_2020_fires2write <- ca_2020_fires %>%
  filter(BurnBndAc > 200000) %>%  # filter for large fires only (200k acres = 80k ha)
  rowwise() %>%
  mutate(bbox = paste(
    round(st_bbox(geometry)[c("xmin","ymin","xmax","ymax")], 2),
    collapse = ",")) %>%
  ungroup()
st_bbox(ca_2020_fires2write)

st_write(ca_2020_fires2write, file.path(fire_out_folder, 'ca_2020_fires.shp'), delete_dsn = TRUE)
