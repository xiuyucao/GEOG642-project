library(tidyverse)
library(sf)

# ------------------------------------ Paths ----------------------------------- #
output_folder <- 'data/intermediate'
plot_folder   <- 'figures'

# ---------------------------------- Variables --------------------------------- #
l2_vars  <- c('rh25', 'rh50', 'rh75', 'rh98', 're_below10m', 'aVDR',
              'cover', 'pai', 'fhd_normal', 'num_detectedmodes')
l4a_vars <- c('agbd')

# ---------------------------------- Functions --------------------------------- #
to_long <- function(pairs_df, vars, time_a, time_b) {
  pairs_df %>%
    st_drop_geometry() %>%
    select(matches(paste0('_(', time_a, '|', time_b, ')$'))) %>%
    pivot_longer(
      everything(),
      names_to      = c('variable', 'time'),
      names_pattern = '(.+)_(\\d)$'
    ) %>%
    filter(variable %in% vars) %>%
    mutate(variable = factor(variable, levels = vars),
           time     = factor(time, levels = c(time_a, time_b)))
}

plot_boxplot <- function(long_df, time_a, time_b, title, ncol = 5) {
  ggplot(long_df, aes(x = time, y = value, fill = time)) +
    geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.2, linewidth = 0.4) +
    facet_wrap(~ variable, scales = 'free_y', ncol = ncol) +
    scale_fill_manual(values = c('0' = '#4E9AF1', '1' = '#E07B54', '2' = '#6DBF82'),
                      labels = c('0' = 'Pre-fire', '1' = 'Post-fire 1', '2' = 'Post-fire 2')) +
    labs(title = title, x = NULL, y = NULL, fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position  = 'bottom',
          strip.text       = element_text(size = 9),
          plot.title       = element_text(size = 12),
          panel.background = element_rect(fill = 'white', color = NA),
          plot.background  = element_rect(fill = 'white', color = NA))
}

# ------------------------------------ Plot ------------------------------------ #
combos <- list(c('0', '1'), c('1', '2'), c('0', '2'))

for (combo in combos) {
  t_a    <- combo[1]
  t_b    <- combo[2]
  suffix <- paste0(t_a, t_b)
  
  l2_pairs  <- read_rds(file.path(output_folder, paste0('l2_pairs_',  suffix, '.rds')))
  l4a_pairs <- read_rds(file.path(output_folder, paste0('l4a_pairs_', suffix, '.rds')))
  
  for (z in c('fire', 'control')) {
    p_l2 <- l2_pairs %>%
      filter(zone == z) %>%
      to_long(l2_vars, t_a, t_b) %>%
      plot_boxplot(t_a, t_b, paste0('L2 canopy metrics — ', str_to_title(z), ' zone | Time ', t_a, ' vs ', t_b), ncol = 5)
    
    p_l4a <- l4a_pairs %>%
      filter(zone == z) %>%
      to_long(l4a_vars, t_a, t_b) %>%
      plot_boxplot(t_a, t_b, paste0('AGBD — ', str_to_title(z), ' zone | Time ', t_a, ' vs ', t_b), ncol = 1)
    
    ggsave(file.path(plot_folder, paste0('l2_boxplot_',  suffix, '_', z, '.png')), p_l2,
           width = 14, height = 6, dpi = 300, bg = 'white')
    ggsave(file.path(plot_folder, paste0('l4a_boxplot_', suffix, '_', z, '.png')), p_l4a,
           width = 4,  height = 4, dpi = 300, bg = 'white')
  }
}