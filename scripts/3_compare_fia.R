library(tidyverse)
library(sf)


# ------------------------------------ Paths ----------------------------------- #
fire_perimeters_file <- 'data/input/fires/ca_2020_fires.shp'
fia_0_file <- 'data/input/fia/plots_2014-2020.rds'
fia_1_file <- 'data/input/fia/plots_2020-2026.rds'
fia_tree_file <- 'data/input/fia/trees.rds'


# ---------------------------------- Get Data ---------------------------------- #
fire <- st_read(fire_perimeters_file) %>%
  filter(Incid_Name == 'August Complex') %>%
  st_transform(crs = 4269)

# Get FIA plots and clip to fire perimeter
fia_14_20 <- read_rds(fia_0_file) %>%
  st_as_sf(coords = c('LON', 'LAT'), crs = 4269) %>%
  st_filter(fire, .predicate = st_within) %>%
  group_by(PLT_CN) %>%
  slice_max(MEASYEAR, n = 1, with_ties = FALSE) %>%
  ungroup()
fia_20_26 <- read_rds(fia_1_file) %>%
  st_as_sf(coords = c('LON', 'LAT'), crs = 4269) %>%
  st_filter(fire, .predicate = st_within) %>%
  group_by(PLT_CN) %>%
  slice_max(MEASYEAR, n = 1, with_ties = FALSE) %>%
  ungroup()
trees <- read_rds(fia_tree_file) %>%
  filter(PLT_CN %in% c(fia_14_20$PLT_CN, fia_20_26$PLT_CN))

# ---------------------------------- Get Area ---------------------------------- #
# Get fire area in acres
fire_area_acres <- fire %>%
  st_transform(crs = 5070) %>%  # project to equal-area CRS (Albers) for accurate area
  st_area() %>%
  as.numeric() * 0.000247105  # convert sq meters to acres

# --------------------------------- Tree Count --------------------------------- #
# Combine plots from both FIA periods
fia_plots <- bind_rows(
  fia_14_20 %>% as.data.frame() %>% select(PLT_CN, MEASYEAR),
  fia_20_26 %>% as.data.frame() %>% select(PLT_CN, MEASYEAR)
)

# Number of plots per year
plot_counts <- fia_plots %>%
  group_by(MEASYEAR) %>%
  summarize(n_plots = n(), .groups = 'drop')

# Tree-level estimate
result <- trees %>%
  inner_join(fia_plots, by = 'PLT_CN') %>%
  inner_join(plot_counts, by = 'MEASYEAR') %>%
  mutate(
    expn       = fire_area_acres / n_plots,
    tree_count = TPA_UNADJ * expn
  )

# Species insight
result_species <- result %>%
  mutate(period = case_when(
    MEASYEAR %in% fia_14_20$MEASYEAR ~ '2014-2020',
    MEASYEAR %in% fia_20_26$MEASYEAR ~ '2020-2026'
  )) %>%
  group_by(period, SPCD) %>%
  summarize(species_count = sum(tree_count, na.rm = TRUE), .groups = 'drop') %>%
  group_by(period) %>%
  mutate(
    pct  = species_count / sum(species_count) * 100,
    SPCD = ifelse(pct < 1, 'Other', as.character(SPCD))
  ) %>%
  group_by(period, SPCD) %>%
  summarize(pct = sum(pct), .groups = 'drop')

ggplot(result_species, aes(x = period, y = pct, fill = SPCD)) +
  geom_bar(stat = 'identity', position = 'stack') +
  labs(
    title = 'Species Composition Before vs After August Complex Fire',
    x     = 'Period',
    y     = 'Proportion (%)',
    fill  = 'Species Code'
  ) +
  theme_minimal()