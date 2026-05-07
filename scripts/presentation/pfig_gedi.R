library(tidyverse)
library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(ggpubr)


# ------------------------------------ Paths ----------------------------------- #
gedi_folder <- 'data/input/gedi'
fire_perimeters_file <- 'data/input/fires/august_complex.shp'
burn_severity_file <- 'data/raw/fire/burn_severity.tif'
plot_crs <- 'EPSG:4326'

# Output folder
out_folder <- 'figures'


# ---------------------------------- Load data --------------------------------- #
# Get fire data
fire <- st_read(fire_perimeters_file) %>%
  filter(Incid_Name == 'August Complex') %>%
  st_transform(crs = plot_crs)
fire_buffer <- st_buffer(fire, dist = 1000)
control     <- st_difference(fire_buffer, fire)

burn_severity <- rast(burn_severity_file)
burn_severity_plot <- burn_severity %>%
  crop(vect(fire_buffer)) %>%
  mask(vect(fire))

# Get GEDI shots
l2_shots  <- read_rds(file.path('data/intermediate', 'l2_shots_subset.rds'))
l4a_shots <- read_rds(file.path('data/intermediate', 'l4a_shots_subset.rds'))


# -------------------------- Reusable transparent theme ------------------------ #
theme_transparent <- theme(
  panel.background      = element_rect(fill = 'transparent', color = NA),
  plot.background       = element_rect(fill = 'transparent', color = NA),
  legend.background     = element_rect(fill = 'transparent', color = NA),
  legend.box.background = element_rect(fill = 'transparent', color = NA),
  panel.grid            = element_blank(),
  axis.text             = element_blank(),
  axis.ticks            = element_blank(),
  legend.position       = 'right'
)


# ------------------------------------ Plot ------------------------------------ #
gedi_xlim <- c(-122.8, -122.75)
gedi_ylim <- c(40.21, 40.17)

zoom_box <- st_as_sfc(st_bbox(c(xmin = gedi_xlim[1], xmax = gedi_xlim[2],
                                ymin = gedi_ylim[2], ymax = gedi_ylim[1]),
                              crs = plot_crs))

fig_fire <- ggplot() +
  geom_spatraster(data = burn_severity_plot) +
  geom_sf(data = control, aes(color = 'Control area'), fill = '#BBDCC1', linewidth = 0.7, alpha = 0.25) +
  geom_sf(data = fire, aes(color = 'Fire perimeter'), fill = NA, linewidth = 1) +
  geom_sf(data = zoom_box, aes(color = 'Zoomed in'), fill = NA, linewidth = 0.7, linetype = 'solid') +
  scale_fill_gradient2(low = '#2166AC', mid = '#F7F7F7', high = '#B2182B', midpoint = 0, name = 'Burn severity', na.value = NA) +
  scale_color_manual(name = NULL,
                     values = c('Control area' = 'forestgreen', 'Fire perimeter' = 'red', 'Zoomed in' = 'black'),
                     breaks = c('Fire perimeter', 'Control area', 'Zoomed in'),
                     guide = guide_legend(override.aes = list(fill      = c(NA, '#BBDCC1', NA),
                                                              alpha     = c(1, 0.25, 1),
                                                              linewidth = c(1, 0.7, 0.7),
                                                              linetype  = c('solid', 'solid', 'solid')))) +
  coord_sf(expand = FALSE) +
  labs(title = '', x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme_transparent

gedi_xlim <- c(-122.8 - 0.006, -122.75 + 0.006)
gedi_ylim <- c(40.21 + 0.006, 40.17 - 0.006)
fig_gedi <- ggplot() +
  geom_sf(data = control, fill = '#BBDCC1', color = '#2F6B3B', linewidth = 0.7, alpha = 0.25) +
  geom_sf(data = fire,    fill = NA,        color = '#B03A2E', linewidth = 1) +
  geom_sf(data = l2_shots, aes(color = time), size = 1, alpha = 1) +
  geom_sf(data = zoom_box, color = 'black', fill = NA, linewidth = 2, linetype = 'solid') +
  scale_color_manual(name = 'Time',
                     values = c('0' = '#4E9AF1', '1' = '#E07B54', '2' = '#6DBF82'),
                     labels = c('0' = 'Pre-fire', '1' = 'Post-fire 1', '2' = 'Post-fire 2')) +
  coord_sf(xlim = gedi_xlim, ylim = gedi_ylim, expand = FALSE) +
  labs(title = '', x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme_transparent


# ------------------------------------ Save ------------------------------------ #
ggsave(file.path(out_folder, 'presentation/fire.png'), fig_fire, width = 6, height = 5, bg = 'transparent')
ggsave(file.path(out_folder, 'presentation/gedi.png'), fig_gedi, width = 5, height = 4, bg = 'transparent')
