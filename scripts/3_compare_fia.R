library(tidyverse)
library(terra)
library(sf)


# ------------------------------------ Paths ----------------------------------- #
fire_file <- 'data/input/fires/august_complex.shp'

fia_input_folder <- 'data/input/fia'
plots0_file <- file.path(fia_input_folder, 'plots_2014-2020.rds')
plots1_file <- file.path(fia_input_folder, 'plots_2020-2026.rds')
trees_file <- file.path(fia_input_folder, 'trees.rds')
ref_species_file <- file.path(fia_input_folder, 'REF_SPECIES.csv')

fnf_folder <- 'data/input/fnf'

out_folder <- 'data/results'


# ---------------------------------- Function ---------------------------------- #
# Get customized unit strata information. Here we only have one estimation unit (i.e., our fire)
# plots: PLOT table queried from FIADB
# estn_unit: the estimation unit (i.e., fire perimeter) in sf format
# time: the year of the FNF data to use for strata assignment.
get_customized_unit_strata <- function(plots, estn_unit, time = '2019'){
  fnf <- rast(file.path(fnf_folder, paste0('fnf_', time, '.tif')))
  plot_location <- select(plots, PLT_CN, LON, LAT) %>%
    distinct()
  
  unit4calc <- estn_unit %>%
    vect() %>%
    project(fnf)
  plots4calc <- plot_location %>%
    vect() %>%
    project(fnf) %>%
    mask(unit4calc)
  fnf4calc <- crop(fnf, unit4calc) %>%
    mask(unit4calc)
  
  # Get plot stratum assignment
  plot_strata <- extract(fnf4calc, plots4calc) %>%
    cbind(as.data.frame(plots4calc)) %>% 
    mutate(STRATUMCD=ifelse(categories=='Forest', 1, 2)) %>%
    rename('STRATUM_DESCR'=categories)
  
  # Get phase 1 and phase 2 point counts in each stratum
  pixel_counts <- freq(fnf4calc) %>%  # phase 1 pixel counts
    pivot_wider(names_from='value', values_from='count') %>%
    mutate(Total=Forest+Nonforest)
  plot_counts <- plot_strata %>%  # phase 2 plot counts
    group_by(STRATUMCD) %>%
    summarise(count=n()) %>%
    rename('P2POINTCNT'=count)
  
  # Get stratum-level adjustment factors for partially non-sampled plots
  plot_cond_prop <- plot_strata %>%
    left_join(plots, by='PLT_CN')
  
  adjustment_factors <- data.frame()
  stratum_list <- unique(plot_strata$STRATUMCD)
  for (stratum in stratum_list){
    plot_cond_prop_stratum <- filter(plot_cond_prop, STRATUMCD==stratum)
    
    sampled <- filter(plot_cond_prop_stratum, COND_STATUS_CD %in% c(1,2,3,4))
    nonsampled <- filter(plot_cond_prop_stratum, COND_STATUS_CD %in% c(5))
    
    sampled_subplot <- sum(sampled$SUBPPROP_UNADJ)
    nonsampled_subplot <- sum(nonsampled$SUBPPROP_UNADJ)
    adj_factor_subplot <- (sampled_subplot + nonsampled_subplot)/sampled_subplot
    
    sampled_microplot <- sum(sampled$MICRPROP_UNADJ)
    nonsampled_microplot <- sum(nonsampled$MICRPROP_UNADJ)
    adj_factor_microplot <- (sampled_microplot + nonsampled_microplot)/sampled_microplot
    
    adj_result <- data.frame(STRATUMCD=stratum, 
                             ADJ_FACTOR_SUBP=adj_factor_subplot,
                             ADJ_FACTOR_MICR=adj_factor_microplot)
    adjustment_factors <- rbind(adjustment_factors, adj_result)
  }
  
  # Get result
  result <- plot_strata %>%
    mutate(P1PNTCNT_EU=pixel_counts$Total,
           P1POINTCNT=ifelse(STRATUMCD==1, pixel_counts$Forest, pixel_counts$Nonforest)) %>%
    mutate(STRATUM_WEIGHT=P1POINTCNT/P1PNTCNT_EU) %>%
    left_join(adjustment_factors, by='STRATUMCD') %>%
    left_join(plot_counts, by='STRATUMCD') %>%
    left_join(plot_location, by='PLT_CN') %>%
    select(PLT_CN, STRATUMCD, STRATUM_DESCR, P1PNTCNT_EU, P1POINTCNT, STRATUM_WEIGHT,
           ADJ_FACTOR_SUBP, ADJ_FACTOR_MICR, P2POINTCNT)
  
  result
}


# ---------------------------------- Get Data ---------------------------------- #
fire <- st_read(fire_file) %>%
  filter(Incid_Name == 'August Complex') %>%
  st_transform(crs = 4269)  # project to FIA CRS

# Get FIA plots and clip to fire perimeter
plots0 <- read_rds(plots0_file) %>%
  st_as_sf(coords = c('LON', 'LAT'), crs = 4269, remove = F) %>%
  st_filter(fire, .predicate = st_within)
plots1 <- read_rds(plots1_file) %>%
  st_as_sf(coords = c('LON', 'LAT'), crs = 4269, remove = F) %>%
  st_filter(fire, .predicate = st_within)
all_trees <- read_rds(trees_file)
ref_species <- read_csv(ref_species_file) %>%
  select(SPCD, GENUS, SPECIES)


# ----------------------------- Estimate Tree Count ---------------------------- #
times <- list(list(plots = plots0, year = '2019', label = 'pre'),
              list(plots = plots1, year = '2021', label = 'post'))
for (time in times){
  # Get tree, plot and strata information
  plots <- get_customized_unit_strata(time$plots, fire, time$year)
  trees <- all_trees %>%
    filter(PLT_CN %in% plots$PLT_CN) %>%
    na.omit()
  strata <- plots %>%
    select(P1PNTCNT_EU, STRATUMCD, STRATUM_DESCR, P1POINTCNT, STRATUM_WEIGHT, P2POINTCNT) %>%
    distinct()
  estn_unit <- strata %>%
    select(P2POINTCNT) %>%
    summarise(P2POINTCNT_EU=sum(P2POINTCNT))
  
  # Get plot level tree count (per acre) by species - partitioned to trees and saplings
  p1.tree_count <- filter(trees, DIA > 5) %>%  # trees >= 5.0 inches d.b.h./d.r.c.
    group_by(PLT_CN, SPCD) %>%
    summarise(count_plot = sum(TPA_UNADJ), .groups = 'drop')
  p1.sapling_count <- filter(trees, DIA <= 5) %>%  # trees < 5.0 inches d.b.h./d.r.c.
    group_by(PLT_CN, SPCD) %>%
    summarise(count_plot = sum(TPA_UNADJ), .groups = 'drop')
  
  # Fill 0 count for species not observed in a plot, apply adjustment factors for partially non-sampled plots
  all_species <- unique(trees$SPCD)
  plot_species_grid <- expand_grid(PLT_CN = plots$PLT_CN, 
                                   SPCD = all_species) %>%
    left_join(plots, by = 'PLT_CN')
  p2.tree_adj <- plot_species_grid %>%
    left_join(p1.tree_count, by = c('PLT_CN', 'SPCD')) %>%
    mutate(count_plot = replace_na(count_plot, 0),
           count_plot_adj = count_plot * ADJ_FACTOR_SUBP)
  p2.sapling_adj <- plot_species_grid %>%
    left_join(p1.sapling_count, by = c('PLT_CN', 'SPCD')) %>%
    mutate(count_plot = replace_na(count_plot, 0),
           count_plot_adj = count_plot * ADJ_FACTOR_MICR)
  
  # Combine trees and saplings for total adjusted species TPA per plot
  p2.all_count <- bind_rows(p2.tree_adj, p2.sapling_adj) %>%
    group_by(STRATUMCD, STRATUM_WEIGHT, PLT_CN, SPCD) %>%
    summarise(total_tpa_plot = sum(count_plot_adj), .groups = 'drop')
  
  # Get weighted mean TPA per species at the unit level (Fire Perimeter)
  p3.unit_tpa_species <- p2.all_count %>%
    group_by(STRATUMCD, STRATUM_WEIGHT, SPCD) %>%
    summarise(tpa_stratum_mean = mean(total_tpa_plot), .groups = 'drop') %>%
    mutate(weighted_tpa = tpa_stratum_mean * STRATUM_WEIGHT) %>%
    group_by(SPCD) %>%
    summarise(unit_mean_tpa = sum(weighted_tpa), .groups = 'drop')
  
  # From mean to total
  fire_area_acres <- as.numeric(st_area(st_transform(fire, crs=5070))) * 0.000247105
  final_species_count <- p3.unit_tpa_species %>%
    mutate(total_estimated_trees = unit_mean_tpa * fire_area_acres) %>%
    left_join(ref_species, by = 'SPCD')
  
  # Write results
  out_file <- file.path(out_folder, paste0('species', time$year, '.rds'))
  write_rds(final_species_count, out_file, compress = 'xz')
}
