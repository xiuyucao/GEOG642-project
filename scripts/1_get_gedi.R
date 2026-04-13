library(tidyverse)
library(sf)


# ---------------------------------- Functions --------------------------------- #
# L2A and L2B have same shots, L4A for 2025 currently is only a subset of L2A
# Filter L2A and L2B shots using variables in L2B
get_l2_shots <- function(gedi_folder, time = '0', write_shp = FALSE){
  time <- match.arg(time, c('0', '1', '2'))
  
  path2file <- file.path(gedi_folder, paste0('gedi_l2b_', time, '.csv'))
  l2_shots <- read.csv(path2file, colClasses = c(shot_number = 'character')) %>%
    filter(l2a_quality_flag == 1, 
           l2b_quality_flag == 1,
           algorithmrun_flag == 1,
           degrade_flag == 0) %>%
    select(shot_number, lon, lat)
  
  if(write_shp){
    shp_path <- file.path('data/temp', paste0('l2_shots_', time, '.shp'))
    l2_shots_sf <- st_as_sf(l2_shots, coords = c('lon', 'lat'), crs = 4326)
    st_write(l2_shots_sf, shp_path, delete_dsn = TRUE)
  }
  
  l2_shots <- select(l2_shots, shot_number)
  l2_shots
}

get_l2a <- function(gedi_folder, shots2use, time = '0'){
  time <- match.arg(time, c('0', '1', '2'))
  
  path2file <- file.path(gedi_folder, paste0('gedi_l2a_', time, '.csv'))
  l2a <- read.csv(path2file, colClasses = c(shot_number = 'character'))
  
  # Get variables - RH25/50/75/98, RE<10m, and aVDR
  # RE<10m: interpolated proportion of waveform returned energy below 10 m height
  # aVDR: 1 - (RH99 – HOMAE)/RH99
  # Might have warnings that R has collapse some rows of x to unique values
  message(paste0('Calculating metrics for L2A time ', time, '...'))
  output <- shots2use %>%
    left_join(l2a, by = 'shot_number') %>%
    filter(rowSums(across(rh0:rh100, ~ . != 0)) > 0) %>%  # remove rows will all 0 rh values (redundant code just to make sure). 
    rowwise() %>%
    mutate(
      metrics = list({
        rh_vals <- c_across(rh0:rh100)
        pct_vals <- 0:100
        rh99 <- rh_vals[100]
        
        # Single approx() call for both metrics
        interp <- suppressWarnings(
          approx(x = rh_vals, y = pct_vals, xout = c(0, 10), rule = 2)$y
        )
        pct_ground  <- interp[1]
        re_below10m <- interp[2]
        
        # HOMAE via direct indexing, no second approx()
        homae_pct <- round((pct_ground + 100) / 2)
        homae     <- rh_vals[homae_pct + 1]
        
        aVDR <- if (is.na(rh99) || is.na(homae) || rh99 == 0) NA_real_
        else 1 - (rh99 - homae) / rh99
        
        list(re_below10m = re_below10m, aVDR = aVDR)
      })
    ) %>%
    unnest_wider(metrics) %>%
    ungroup() %>%
    select(shot_number, rh25, rh50, rh75, rh98, re_below10m, aVDR, delta_time, lat, lon, elev_lowestmode)
  
  output
}


get_l2b <- function(gedi_folder, shots2use, time = '0'){
  time <- match.arg(time, c('0', '1', '2'))
  
  path2file <- file.path(gedi_folder, paste0('gedi_l2b_', time, '.csv'))
  l2b <- read.csv(path2file, colClasses = c(shot_number = 'character'))
  
  # Get variables: cover, pai, num_detectedmodes, and fhd_normal
  message(paste0('Getting metrics for L2B time ', time, '...'))
  output <- shots2use %>%
    left_join(l2b, by = 'shot_number') %>%
    select(shot_number, cover, pai, num_detectedmodes, fhd_normal)
  
  output
}


get_l4a <- function(gedi_folder, l2_shots2use, time = '0'){
  time <- match.arg(time, c('0', '1', '2'))
  
  path2file <- file.path(gedi_folder, paste0('gedi_l4a_', time, '.csv'))
  l4a <- read.csv(path2file, colClasses = c(shot_number = 'character'))
  
  # Get AGBD variables
  message(paste0('Getting metrics for L4A time ', time, '...'))
  output <- l2_shots2use %>%
    left_join(l4a, by = 'shot_number') %>%
    filter(algorithm_run_flag == 1,
           agbd != -9999,
           l4_quality_flag == 1,
           lcd_landsat_water_persistence < 10) %>%
    filter(lcd_pft_class %in% c(1, 2, 3, 4, 5, 9)) %>%
    select(shot_number, agbd, agbd_pi_lower, agbd_pi_upper, agbd_se, lat, lon, elev_lowestmode)
  
  output
}


# ------------------------------------ Paths ----------------------------------- #
# GEDI folder and take a look at the files inside
gedi_folder <- 'data/raw/gedi'
gedi_files <- list.files(gedi_folder, full.names=T)
gedi_files <- gedi_files[grepl('.csv$', gedi_files)]

# Output folder
out_folder <- 'data/input/gedi'


# ---------------------------------- Get Data ---------------------------------- #
# ---------------------------------- Write clean L2 shots
for (t in c('0', '1', '2')) {
  l2_shots <- get_l2_shots(gedi_folder, time = t)
  l2a <- get_l2a(gedi_folder, l2_shots, time = t)
  l2b <- get_l2b(gedi_folder, l2_shots, time = t)
  
  l2 <- left_join(l2a, l2b, by = 'shot_number')
  l4a <- get_l4a(gedi_folder, l2_shots, time = t)
  
  write_rds(l2, file.path(out_folder, paste0('gedi_l2_', t, '.rds')), compress = 'xz')
  write_rds(l4a, file.path(out_folder, paste0('gedi_l4a_', t, '.rds')), compress = 'xz')
}
