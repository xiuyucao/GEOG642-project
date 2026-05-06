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
spatial_thin <- function(pairs_df, min_dist_m = 200) {
  df       <- pairs_df %>% st_transform(32610)
  selected <- logical(nrow(df))
  selected[1] <- TRUE
  pts <- st_geometry(df)
  
  for (i in seq(2, nrow(df))) {
    dists <- as.numeric(st_distance(pts[i], pts[selected]))
    if (min(dists) >= min_dist_m) selected[i] <- TRUE
  }
  
  df[selected, ] %>% st_transform(st_crs(pairs_df))
}

paired_t_thinned <- function(pairs_df, vars, time_a, time_b, min_dist_m = 200) {
  thinned <- spatial_thin(pairs_df, min_dist_m)
  
  map_dfr(vars, function(var) {
    col_a <- paste0(var, '_', time_a)
    col_b <- paste0(var, '_', time_b)
    
    if (!col_a %in% names(thinned) || !col_b %in% names(thinned)) return(NULL)
    
    diffs <- thinned[[col_b]] - thinned[[col_a]]
    diffs <- diffs[!is.na(diffs)]
    
    if (length(diffs) < 3) {
      warning(sprintf('Too few observations for %s after thinning', var))
      return(NULL)
    }
    
    tt <- t.test(diffs, mu = 0)
    
    tibble(
      variable   = var,
      n_original = nrow(pairs_df),
      n_thinned  = nrow(thinned),
      mean_diff  = tt$estimate,
      sd_diff    = sd(diffs),
      t_stat     = tt$statistic,
      df         = tt$parameter,
      p_value    = tt$p.value,
      ci_lower   = tt$conf.int[1],
      ci_upper   = tt$conf.int[2]
    )
  })
}

plot_ci <- function(results_df, dataset_label, title, sig_threshold = 0.05) {
  results_df %>%
    mutate(
      variable  = factor(variable, levels = rev(unique(variable))),
      sig       = ifelse(p_adj < sig_threshold, 'Significant', 'Non-significant'),
      time_label = paste0('Time ', time_a, ' → ', time_b)
    ) %>%
    ggplot(aes(x = mean_diff, y = variable, color = sig)) +
    geom_vline(xintercept = 0, linetype = 'dashed', color = 'grey50', linewidth = 0.4) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                   height = 0.3, linewidth = 0.5) +
    geom_point(size = 2) +
    facet_wrap(~ time_label, nrow = 1) +
    scale_color_manual(values = c('Significant'     = '#D64E3B',
                                  'Non-significant' = '#7B9FD4')) +
    labs(title    = title,
         subtitle = paste0('n thinned shown per panel | FDR-adjusted p < ', sig_threshold, ' = significant'),
         x        = 'Mean difference (Time B \u2212 Time A)',
         y        = NULL,
         color    = NULL) +
    geom_text(aes(x = Inf, label = paste0('n = ', n_thinned)),
              hjust = 1.1, vjust = -0.5, size = 2.8, color = 'grey40', show.legend = FALSE) +
    theme_minimal(base_size = 11) +
    theme(legend.position  = 'bottom',
          strip.text       = element_text(size = 10),
          plot.title       = element_text(size = 12),
          panel.background = element_rect(fill = 'white', color = NA),
          plot.background  = element_rect(fill = 'white', color = NA))
}

# ------------------------------------ Run ------------------------------------- #
combos <- list(c('0', '1'), c('1', '2'), c('0', '2'))

all_results <- map_dfr(combos, function(combo) {
  t_a    <- combo[1]
  t_b    <- combo[2]
  suffix <- paste0(t_a, t_b)
  
  l2_pairs  <- read_rds(file.path(output_folder, paste0('l2_pairs_',  suffix, '.rds')))
  l4a_pairs <- read_rds(file.path(output_folder, paste0('l4a_pairs_', suffix, '.rds')))
  
  map_dfr(c('fire', 'control'), function(z) {
    bind_rows(
      paired_t_thinned(filter(l2_pairs,  zone == z), l2_vars,  t_a, t_b) %>% mutate(dataset = 'L2'),
      paired_t_thinned(filter(l4a_pairs, zone == z), l4a_vars, t_a, t_b) %>% mutate(dataset = 'L4A')
    ) %>%
      mutate(zone = z, time_a = t_a, time_b = t_b)
  })
}) %>%
  mutate(p_adj = p.adjust(p_value, method = 'fdr')) %>%
  select(dataset, zone, time_a, time_b, variable,
         n_original, n_thinned, mean_diff, sd_diff,
         t_stat, df, p_value, p_adj, ci_lower, ci_upper)

write_csv(all_results, file.path(output_folder, 'compare_paired_t_results.csv'))

# ------------------------------------ Plot ------------------------------------ #
for (z in c('fire', 'control')) {
  # L2: all time combos in one figure, faceted by time pair
  p_l2 <- all_results %>%
    filter(dataset == 'L2', zone == z) %>%
    plot_ci('L2', paste0('L2 canopy metrics — ', str_to_title(z), ' zone'))
  
  ggsave(file.path(plot_folder, paste0('ttest_l2_ci_', z, '.png')), p_l2,
         width = 12, height = 6, dpi = 300, bg = 'white')
  
  # L4A: one figure per zone
  p_l4a <- all_results %>%
    filter(dataset == 'L4A', zone == z) %>%
    plot_ci('L4A', paste0('AGBD — ', str_to_title(z), ' zone'))
  
  ggsave(file.path(plot_folder, paste0('ttest_l4a_ci_', z, '.png')), p_l4a,
         width = 8, height = 3, dpi = 300, bg = 'white')
}