library(tidyverse)
library(terra)
library(sf)


# ------------------------------------ Paras ----------------------------------- #
nlcd_folder         <- 'data/raw/nlcd'
nlcd_2019_file      <- 'Annual_NLCD_LndCov_2019_CU_C1V1/Annual_NLCD_LndCov_2019_CU_C1V1.tif'
nlcd_2021_file      <- 'Annual_NLCD_LndCov_2021_CU_C1V1/Annual_NLCD_LndCov_2021_CU_C1V1.tif'
output_folder       <- 'data/input/fnf'
fire_perimeter_file <- 'data/input/fires/august_complex.shp'

# NLCD forest class codes (41 = Deciduous, 42 = Evergreen, 43 = Mixed Forest)
FOREST_CLASSES <- c(41, 42, 43)

nlcd_files <- list(
  `2019` = file.path(nlcd_folder, nlcd_2019_file),
  `2021` = file.path(nlcd_folder, nlcd_2021_file)
)


# ---------------------------------- Load Fire Perimeter ----------------------- #
fire_perimeter <- st_read(fire_perimeter_file)


# ---------------------------------- Process Each Year ------------------------- #
for (year in c(2019, 2021)) {
  message("Processing year: ", year)
  
  # Get NLCD in fire perimeter
  nlcd <- rast(nlcd_files[[as.character(year)]])
  fire_vect <- vect(fire_perimeter) %>%
    project(nlcd)
  nlcd_fire <- nlcd %>%
    crop(fire_vect) %>%
    mask(fire_vect)
  
  # Get FNF mask (1 = forest, 0 = non-forest)
  fnf <- ifel(nlcd_fire %in% FOREST_CLASSES, 1L, 0L) %>%
    mask(fire_vect)
  
  # Write raster
  out_file <- file.path(output_folder, paste0("fnf_", year, ".tif"))
  writeRaster(fnf, out_file, overwrite = T, datatype = 'INT1U')
}
