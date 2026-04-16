library(tidyverse)
library(ggpubr)
source('scripts/functions/symlog.R')


# ------------------------------------ Paths ----------------------------------- #
sp2019_file <- 'data/results/species2019.rds'
sp2021_file <- 'data/results/species2021.rds'
out_folder <- 'figures'


# ---------------------------------- Load Data --------------------------------- #
sp2019 <- read_rds(sp2019_file) %>%
  mutate(year = 2019)
sp2021 <- read_rds(sp2021_file) %>%
  mutate(year = 2021)

# Get combined table
# Fill missing species-year combos, and order by total abundance across both years
sp <- bind_rows(sp2019, sp2021) %>%
  mutate(species_label = paste(GENUS, SPECIES)) %>%
  select(GENUS, SPECIES, species_label, year, total_estimated_trees, unit_mean_tpa) %>%
  complete(nesting(GENUS, SPECIES, species_label), year=c(2019, 2021),
           fill = list(total_estimated_trees = 0, unit_mean_tpa = 0)) %>%
  group_by(species_label) %>%
  mutate(total_abundance_all_years = sum(total_estimated_trees)) %>%
  ungroup() %>%
  arrange(desc(total_abundance_all_years)) %>%
  mutate(species_label = fct_reorder(species_label, total_abundance_all_years))


# ------------------------------------ Plot ------------------------------------ #
# ------------------------------------ Bar chart comparing abundance
data2plot <- sp %>%
  mutate(n_log = symlog(total_estimated_trees))

fig_ntrees <- ggplot() +
  theme_minimal() +
  geom_col(data = data2plot, 
           aes(y = species_label, x = total_estimated_trees, 
               fill = factor(year, levels = c(2021, 2019))),
           position = position_dodge(width = 0.8), width = 0.7) +
  labs(fill = NULL)

# ------------------------------------ Proportional stacked bar (TPA)
data2plot <- sp %>%
  group_by(year) %>%
  mutate(prop_tpa = unit_mean_tpa / sum(unit_mean_tpa)) %>%
  ungroup() %>%
  mutate(species_label = fct_reorder(species_label, prop_tpa, sum))

fig_proportion <- ggplot() +
  theme_minimal() +
  geom_col(data = data2plot,
           aes(x = factor(year), y = prop_tpa, fill = species_label),
           position = "fill", width = 0.5) +
  labs(x = "Year", y = "%", fill = NULL)


# ----------------------------------- Write ------------------------------------ #
fig <- ggarrange(fig_ntrees, fig_proportion, 
                 nrow = 2, ncol = 1, labels = c('a', 'b'))
ggsave(file.path(out_folder, 'figx_species_change.png'), fig, width = 12, height = 8, bg = 'white')
