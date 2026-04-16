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

# Category levels and colors
FNF_LEVELS <- data.frame(values = c(1, 2),
                         categories = c('Forest', 'Nonforest'))
FNF_COLTAB <- FNF_LEVELS %>%
  mutate(color = c('forestgreen', 'grey85')) %>%
  select(values, color)

nlcd_files <- list(
  `2019` = file.path(nlcd_folder, nlcd_2019_file),
  `2021` = file.path(nlcd_folder, nlcd_2021_file)
)


# ---------------------------------- Load Fire Perimeter ----------------------- #
fire_perimeter <- st_read(fire_perimeter_file)


# ---------------------------------- Process Each Year ------------------------- #
for (year in c(2019, 2021)) {
  message("Processing year: ", year)
  
  # Load NLCD and clip to fire perimeter
  nlcd <- rast(nlcd_files[[as.character(year)]])
  fire_vect <- vect(fire_perimeter) %>%
    project(crs(nlcd))
  nlcd_fire <- nlcd %>%
    crop(fire_vect) %>%
    mask(fire_vect)
  
  # Reclassify: Forest (41, 42, 43) --> 1, Nonforest --> 2
  fnf <- ifel(nlcd_fire %in% FOREST_CLASSES, 1L, 2L) %>%
    mask(fire_vect)
  
  # Assign levels and color table
  levels(fnf)  <- FNF_LEVELS
  coltab(fnf)  <- FNF_COLTAB
  names(fnf)   <- 'categories'
  
  # Write raster
  out_file <- file.path(output_folder, paste0("fnf_", year, ".tif"))
  writeRaster(fnf, out_file, overwrite = TRUE, datatype = 'INT1U')
}
