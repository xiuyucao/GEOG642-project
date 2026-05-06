library(tidyverse)
library(nlme)
library(broom.mixed)
library(sf)
library(ggpubr)
source('scripts/functions/convert_pairs2long.R')


# -------------------------------- Data Read In -------------------------------- #
gedi_folder <- 'data/intermediate'

# Get different pairs
p_struct_fire <- file.path(gedi_folder, 'l2_pairs_01.rds') %>% read_rds()
p_struct_recover <- file.path(gedi_folder, 'l2_pairs_12.rds') %>% read_rds()
p_agbd_fire <- file.path(gedi_folder, 'l4a_pairs_01.rds') %>% read_rds()
p_agbd_recover <- file.path(gedi_folder, 'l4a_pairs_12.rds') %>% read_rds()


# ----------------------------------- Variables -------------------------------- #
vars_structure <- c('rh25', 'rh50', 'rh75', 'rh98', 're_below10m', 'aVDR', 'cover', 'pai', 'fhd_normal')
vars_agbd <- 'agbd'


# -------------------------------- Tool functions ------------------------------ #
# Extract fixed effects
extract_effects <- function(model, metric_name) {
  broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE, conf.level = 0.95) %>%
    filter(term != "(Intercept)") %>%
    mutate(metric = metric_name,
           effect = case_when(str_detect(term, ":")           ~ "Interaction",
                              str_starts(term, "treatment")   ~ "Burned",
                              str_starts(term, "time")        ~ "Timing",
                              TRUE                             ~ term),
           effect = factor(effect, levels = c("Burned", "Timing", "Interaction")),
           significant = p.value <= 0.05) %>%
    select(metric, effect, estimate, std.error, conf.low, conf.high, p.value, significant)
}

# Fit one metric
get_mod <- function(pairs2use, var_name, t1='0', t2='1'){
  pl <- convert_pairs2long(pairs2use, var=var_name, zscore = TRUE, downsample = TRUE,
                           time1=t1, time2=t2, treatment_col='zone')
  asp <- lme(value ~ treatment * time, data = pl, random = ~ 1 | index, control = lmeControl(opt = "optim"))
  sp  <- update(asp, correlation = corLin(form = ~ x + y, nugget = FALSE))
  extract_effects(sp, metric_name=var_name)
}

# Fit all metrics for a pairs table
get_mod_loop <- function(pairs2use, var_vect, t1='0', t2='1'){
  all_effects <- map_dfr(var_vect, ~ get_mod(pairs2use, .x, t1, t2)) %>%
    mutate(metric = factor(metric, levels = var_vect))  # Lock in metric ordering for later plotting
}

# Plot function
plot_effects <- function(effects_df, title='', subtitle='') {
  ggplot() +
    theme_minimal() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    geom_pointrange(data=effects_df, aes(x = estimate, y = metric, color = effect, xmin = conf.low, xmax = conf.high),
                    position = position_dodge(width = 0.2),
                    size = 0.2, linewidth = 0.4) +
    scale_color_manual(values = c("Burned" = "#E69F00", "Timing" = "#009E73", "Interaction" = "#56B4E9")) +
    scale_y_discrete(limits = rev) +
    coord_cartesian(xlim = c(-0.7, 0.7)) +
    labs(x = 'Normalized effect (95% CIs)', y = '', color= '',
         title = title, subtitle = subtitle) +
    theme(legend.position = "bottom", panel.grid.major.y = element_blank())
}


# ------------------------------------ Main ------------------------------------ #
res_struct_fire <- get_mod_loop(p_struct_fire, vars_structure, t1='0', t2='1')
res_agbd_fire <- get_mod_loop(p_agbd_fire, vars_agbd, t1='0', t2='1')
res_fire <- rbind(res_struct_fire, res_agbd_fire)

res_struct_recover <- get_mod_loop(p_struct_recover, vars_structure, t1='1', t2='2')
res_agbd_recover <- get_mod_loop(p_agbd_recover, vars_agbd, t1='1', t2='2')
res_recover <- rbind(res_struct_recover, res_agbd_recover)

# Plot
fig_fire <- plot_effects(res_fire)
fig_recover <- plot_effects(res_recover)
fig <- ggarrange(fig_fire, fig_recover,
                 ncol=2,labels=c('(a)', '(b)'),
                 common.legend = TRUE, legend = 'bottom')
ggsave('figures/fig3_gedi_changes.png', fig, width = 6, height = 7, bg = 'white')
