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

to_diff_long <- function(pairs_df, vars, time_a, time_b) {
  
  df <- pairs_df %>% 
    st_drop_geometry()
  
  # Keep zone information
  zone_df <- df %>% 
    select(zone)
  
  # For each variable, compute difference: time_b - time_a
  diff_df <- map_dfc(vars, function(var) {
    col_a <- paste0(var, '_', time_a)
    col_b <- paste0(var, '_', time_b)
    
    if (!col_a %in% names(df) || !col_b %in% names(df)) return(NULL)
    
    tibble(!!var := df[[col_b]] - df[[col_a]])
  })
  
  bind_cols(zone_df, diff_df) %>%
    pivot_longer(
      cols = all_of(vars),
      names_to  = 'variable',
      values_to = 'diff'
    ) %>%
    filter(variable %in% vars) %>%
    mutate(
      variable = factor(variable, levels = vars),
      zone = factor(zone, levels = c('control', 'fire'))
    )
}

plot_diff_boxplot <- function(diff_long_df, time_a, time_b, title, ncol = 5) {
  
  ggplot(diff_long_df, aes(x = zone, y = diff, fill = zone)) +
    geom_hline(
      yintercept = 0,
      linetype = 'dashed',
      color = 'grey50',
      linewidth = 0.4
    ) +
    geom_boxplot(
      outlier.size = 0.4,
      outlier.alpha = 0.2,
      linewidth = 0.4
    ) +
    facet_wrap(~ variable, scales = 'free', ncol = ncol) +
    labs(
      title = title,
      x = NULL,
      y = paste0('Difference (Time ', time_b, ' \u2212 Time ', time_a, ')'),
      fill = NULL
    ) +
    scale_fill_manual(
      values = c(
        'control' = '#7B9FD4',
        'fire'    = '#D47B7B'
      ),
      labels = c(
        'control' = 'Control',
        'fire'    = 'Fire'
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text        = element_text(size = 9),
      plot.title        = element_text(size = 12),
      panel.background  = element_rect(fill = 'white', color = NA),
      plot.background   = element_rect(fill = 'white', color = NA),
      legend.position   = 'bottom'
    )
}

# ------------------------------------ Plot ------------------------------------ #

combos <- list(c('0', '1'), c('1', '2'), c('0', '2'))

for (combo in combos) {
  
  t_a    <- combo[1]
  t_b    <- combo[2]
  suffix <- paste0(t_a, t_b)
  
  l2_pairs  <- read_rds(file.path(output_folder, paste0('l2_pairs_',  suffix, '.rds')))
  l4a_pairs <- read_rds(file.path(output_folder, paste0('l4a_pairs_', suffix, '.rds')))
  
  p_l2 <- l2_pairs %>%
    filter(zone %in% c('fire', 'control')) %>%
    to_diff_long(l2_vars, t_a, t_b) %>%
    plot_diff_boxplot(
      t_a, t_b,
      paste0('L2 canopy metrics (Δ) — Fire vs Control | Time ', t_a, ' vs ', t_b),
      ncol = 5
    )
  
  p_l4a <- l4a_pairs %>%
    filter(zone %in% c('fire', 'control')) %>%
    to_diff_long(l4a_vars, t_a, t_b) %>%
    plot_diff_boxplot(
      t_a, t_b,
      paste0('AGBD (Δ) — Fire vs Control | Time ', t_a, ' vs ', t_b),
      ncol = 1
    )
  
  ggsave(
    file.path(plot_folder, paste0('compare_l2_boxplot_', suffix, '_fire_control.png')),
    p_l2,
    width = 14,
    height = 6,
    dpi = 300,
    bg = 'white'
  )
  
  ggsave(
    file.path(plot_folder, paste0('compare_l4a_boxplot_', suffix, '_fire_control.png')),
    p_l4a,
    width = 4,
    height = 4,
    dpi = 300,
    bg = 'white'
  )
}