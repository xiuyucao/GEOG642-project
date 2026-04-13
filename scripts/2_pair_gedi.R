library(tidyverse)
library(terra)
library(sf)


# ---------------------------------- Functions --------------------------------- #
get_pairs <- function(sf_a, sf_b, max_dist = 45, max_elev_diff = 10) {
  nn <- st_nearest_feature(sf_a, sf_b)
  dist <- as.numeric(st_distance(sf_a, sf_b[nn, ], by_element = TRUE))
  elev_diff <- abs(sf_a$elev_lowestmode - sf_b$elev_lowestmode[nn])
  
  tibble(shot_a = sf_a$shot_number,
         shot_b = sf_b$shot_number[nn],
         dist = dist,
         elev_diff = elev_diff) %>%
    filter(dist <= max_dist, elev_diff <= max_elev_diff)
}

join_pair_attrs <- function(pairs, sf_a, sf_b, time_a, time_b) {
  attrs_a <- sf_a %>% st_drop_geometry() %>% rename_with(~ paste0(., '_', time_a), -shot_number)
  attrs_b <- sf_b %>% st_drop_geometry() %>% rename_with(~ paste0(., '_', time_b), -shot_number)
  
  # Compute mean location between paired shots
  coords_a <- st_coordinates(sf_a)[match(pairs$shot_a, sf_a$shot_number), ]
  coords_b <- st_coordinates(sf_b)[match(pairs$shot_b, sf_b$shot_number), ]
  mean_coords <- (coords_a + coords_b) / 2
  
  pairs %>%
    left_join(attrs_a, by = c('shot_a' = 'shot_number')) %>%
    left_join(attrs_b, by = c('shot_b' = 'shot_number')) %>%
    mutate(x = mean_coords[, 1], y = mean_coords[, 2]) %>%
    st_as_sf(coords = c('x', 'y'), crs = 5070)
}

add_fire_attrs <- function(pairs_sf, fire, control, burn_severity_binary) {
  in_fire <- as.vector(st_within(pairs_sf, fire, sparse = FALSE))
  in_control <- as.vector(st_within(pairs_sf, control, sparse = FALSE))
  severity <- extract(burn_severity_binary, vect(pairs_sf))[[2]]
  
  pairs_sf %>%
    mutate(zone = case_when(in_fire ~ 'fire', in_control ~ 'control', TRUE ~ NA_character_)) %>%
    mutate(burn_severity = severity)
}


# ------------------------------------ Paths ----------------------------------- #
gedi_folder <- 'data/input/gedi'
fire_perimeters_file <- 'data/input/fires/ca_2020_fires.shp'
burn_severity_file <- 'data/raw/fire/burn_severity.tif'
output_folder <- 'data/intermediate'


# ---------------------------------- Get Fire ---------------------------------- #
fire <- st_read(fire_perimeters_file) %>%
  filter(Incid_Name == 'August Complex') %>%
  st_transform(crs = 5070)
fire_buffer <- st_buffer(fire, dist = 1000)
control     <- st_difference(fire_buffer, fire)

burn_severity <- rast(burn_severity_file) %>%
  project('EPSG:5070')
burn_severity_binary <- classify(burn_severity,
                                 rbind(c(-Inf, 1, 0),
                                       c(1, Inf, 1)))


# ------------------------------- Get GEDI Pairs ------------------------------- #
# Load GEDI L2 from time 0, 1, 2
l2_sf <- lapply(c('0', '1', '2'), function(t) {
  read_rds(file.path(gedi_folder, paste0('gedi_l2_', t, '.rds'))) %>%
    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
    st_transform(crs = 5070)
})
names(l2_sf) <- c('0', '1', '2')

# Keep only shots within fire_buffer
l2_sf <- lapply(l2_sf, function(sf) {
  sf[st_within(sf, fire_buffer, sparse = FALSE), ]
})

# Get pairs for each time combination
pairs_01 <- get_pairs(l2_sf[['0']], l2_sf[['1']]) %>%
  join_pair_attrs(l2_sf[['0']], l2_sf[['1']], '0', '1') %>%
  add_fire_attrs(fire, control, burn_severity_binary) %>%
  write_rds(file.path(output_folder, 'gedi_pairs_01.rds'))
pairs_12 <- get_pairs(l2_sf[['1']], l2_sf[['2']]) %>%
  join_pair_attrs(l2_sf[['1']], l2_sf[['2']], '1', '2') %>%
  add_fire_attrs(fire, control, burn_severity_binary) %>%
  write_rds(file.path(output_folder, 'gedi_pairs_12.rds'))
pairs_02 <- get_pairs(l2_sf[['0']], l2_sf[['2']]) %>%
  join_pair_attrs(l2_sf[['0']], l2_sf[['2']], '0', '2') %>%
  add_fire_attrs(fire, control, burn_severity_binary) %>%
  write_rds(file.path(output_folder, 'gedi_pairs_02.rds'))
