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
    select(shot_number, rh25, rh50, rh75, rh98, re_below10m, aVDR, delta_time, lat, lon)
  
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


# ------------------------------------ Paths ----------------------------------- #
# GEDI folder and take a look at the files inside
gedi_folder <- 'data/raw/gedi'
gedi_files <- list.files(gedi_folder, full.names=T)
gedi_files <- gedi_files[grepl('.csv$', gedi_files)]

# Output folder
out_folder <- 'data/input/gedi'


# ---------------------------------- Get Data ---------------------------------- #
# ---------------------------------- Write clean L2 shots
# Get filtered L2 shots
l2_shots0 <- get_l2_shots(gedi_folder, time = '0', write_shp = F)
l2_shots1 <- get_l2_shots(gedi_folder, time = '1', write_shp = F)
l2_shots2 <- get_l2_shots(gedi_folder, time = '2', write_shp = F)

# Get L2A
l2a0 <- get_l2a(gedi_folder, l2_shots0, time = '0')
l2a1 <- get_l2a(gedi_folder, l2_shots1, time = '1')
l2a2 <- get_l2a(gedi_folder, l2_shots2, time = '2')

# Get L2B
l2b0 <- get_l2b(gedi_folder, l2_shots0, time = '0')
l2b1 <- get_l2b(gedi_folder, l2_shots1, time = '1')
l2b2 <- get_l2b(gedi_folder, l2_shots2, time = '2')

# Join and write l2 shots
l2_0 <- left_join(l2a0, l2b0, by = 'shot_number')
write_rds(l2_0, file.path(out_folder, 'l2_0.rds'), compress = 'xz')
l2_1 <- left_join(l2a1, l2b1, by = 'shot_number')
write_rds(l2_1, file.path(out_folder, 'l2_1.rds'), compress = 'xz')
l2_2 <- left_join(l2a2, l2b2, by = 'shot_number')
write_rds(l2_2, file.path(out_folder, 'l2_2.rds'), compress = 'xz')
