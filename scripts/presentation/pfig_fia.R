library(tidyverse)
library(sf)
library(ggplot2)


# ------------------------------------ Paths ----------------------------------- #
fia_folder <- 'data/input/fia'
fire_perimeters_file <- 'data/input/fires/august_complex.shp'
plot_crs <- 'EPSG:4326'

# Output folder
out_folder <- 'figures'


# ---------------------------------- Load data --------------------------------- #
# Get fire data
fire <- st_read(fire_perimeters_file) %>%
  filter(Incid_Name == 'August Complex') %>%
  st_transform(crs = plot_crs)

# Get FIA plots and clip to fire perimeter
fia_14_20 <- read_rds(file.path(fia_folder, 'plots_2014-2020.rds')) %>%
  st_as_sf(coords = c('LON', 'LAT'), crs = 4269) %>%
  st_transform(crs = plot_crs) %>%
  st_filter(fire, .predicate = st_within)
fia_20_26 <- read_rds(file.path(fia_folder, 'plots_2020-2026.rds')) %>%
  st_as_sf(coords = c('LON', 'LAT'), crs = 4269) %>%
  st_transform(crs = plot_crs) %>%
  st_filter(fire, .predicate = st_within)


# ------------------------------------ Plot ------------------------------------ #
theme_transparent <- theme(
  panel.background      = element_rect(fill = 'transparent', color = NA),
  plot.background       = element_rect(fill = 'transparent', color = NA),
  legend.background     = element_rect(fill = 'transparent', color = NA),
  legend.box.background = element_rect(fill = 'transparent', color = NA),
  panel.grid.major      = element_blank(),
  panel.grid.minor      = element_blank(),
  axis.text             = element_blank(),
  axis.ticks            = element_blank(),
  legend.position       = 'right'
)

fig_fia <- ggplot() +
  geom_sf(data = fire,    fill = NA,        color = '#B03A2E', linewidth = .5) +
  geom_sf(data = fia_14_20, aes(color = '2014-2020'), size = 1) +
  geom_sf(data = fia_20_26, aes(color = '2020-2026'), size = 1) +
  scale_color_manual(name = 'FIA Plots',
                     values = c('2014-2020' = '#4E9AF1', '2020-2026' = '#E07B54')) +
  labs(title = '', x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme_transparent


# ------------------------------------ Save ------------------------------------ #
ggsave(file.path(out_folder, 'presentation/fia.png'), fig_fia,
       width = 4, height = 4, bg = 'transparent')
