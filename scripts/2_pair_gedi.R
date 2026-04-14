library(tidyverse)
library(terra)
library(sf)


# ------------------------------------ Paths ----------------------------------- #
gedi_folder          <- 'data/input/gedi'
fire_perimeters_file <- 'data/input/fires/ca_2020_fires.shp'
burn_severity_file   <- 'data/raw/fire/burn_severity.tif'
output_folder        <- 'data/intermediate'


# ---------------------------------- Functions --------------------------------- #
# Load GEDI L2 and L4A from time 0, 1, 2 and filter to fire_buffer
load_gedi <- function(prefix) {
  lapply(c('0', '1', '2'), function(t) {
    read_rds(file.path(gedi_folder, paste0(prefix, t, '.rds'))) %>%
      st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
      st_transform(crs = 5070) %>%
      filter(as.vector(st_within(., fire_buffer, sparse = FALSE)))
  }) %>% setNames(c('0', '1', '2'))
}

# Get GEDI pairs based on the filters
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

# Get all variables from the original data for the pairs
join_pair_attrs <- function(pairs, sf_a, sf_b, time_a, time_b) {
  attrs_a <- sf_a %>% st_drop_geometry() %>% rename_with(~ paste0(., '_', time_a), -shot_number)
  attrs_b <- sf_b %>% st_drop_geometry() %>% rename_with(~ paste0(., '_', time_b), -shot_number)
  
  coords_a <- st_coordinates(sf_a)[match(pairs$shot_a, sf_a$shot_number), ]
  coords_b <- st_coordinates(sf_b)[match(pairs$shot_b, sf_b$shot_number), ]
  mean_coords <- (coords_a + coords_b) / 2
  
  pairs %>%
    left_join(attrs_a, by = c('shot_a' = 'shot_number')) %>%
    left_join(attrs_b, by = c('shot_b' = 'shot_number')) %>%
    mutate(x = mean_coords[, 1], y = mean_coords[, 2]) %>%
    st_as_sf(coords = c('x', 'y'), crs = 5070)
}

# Get the fire/control zone and burn severity information for each pair
add_fire_attrs <- function(pairs_sf, fire, control, burn_severity_binary) {
  in_fire    <- as.vector(st_within(pairs_sf, fire,    sparse = FALSE))
  in_control <- as.vector(st_within(pairs_sf, control, sparse = FALSE))
  severity   <- extract(burn_severity_binary, vect(pairs_sf))[[2]]
  
  pairs_sf %>%
    mutate(zone = case_when(in_fire ~ 'fire', in_control ~ 'control', TRUE ~ NA_character_),
           burn_severity = severity)
}


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
l2_sf  <- load_gedi('gedi_l2_')
l4a_sf <- load_gedi('gedi_l4a_')

# Save filtered GEDI points for visualization
bind_rows(l2_sf,  .id = 'time') %>% write_rds(file.path(output_folder, 'l2_shots_subset.rds'))
bind_rows(l4a_sf, .id = 'time') %>% write_rds(file.path(output_folder, 'l4a_shots_subset.rds'))

# Get pairs for each time combination
for (combo in list(c('0', '1'), c('1', '2'), c('0', '2'))) {
  t_a <- combo[1]; t_b <- combo[2]
  suffix <- paste0(t_a, t_b)
  
  # L2 pairs
  get_pairs(l2_sf[[t_a]], l2_sf[[t_b]]) %>%
    join_pair_attrs(l2_sf[[t_a]], l2_sf[[t_b]], t_a, t_b) %>%
    add_fire_attrs(fire, control, burn_severity_binary) %>%
    write_rds(file.path(output_folder, paste0('l2_pairs_', suffix, '.rds')))
  
  # L4A pairs
  get_pairs(l4a_sf[[t_a]], l4a_sf[[t_b]]) %>%
    join_pair_attrs(l4a_sf[[t_a]], l4a_sf[[t_b]], t_a, t_b) %>%
    add_fire_attrs(fire, control, burn_severity_binary) %>%
    write_rds(file.path(output_folder, paste0('l4a_pairs_', suffix, '.rds')))
}
