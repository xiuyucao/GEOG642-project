library(tidyverse)
library(nlme)
library(broom.mixed)
source('scripts/functions/convert_pairs2long.R')


# ------------------------------------ Paths ----------------------------------- #
gedi_folder <- 'data/intermediate'

pairs2use <- file.path(gedi_folder, 'l2_pairs_01.rds') %>%
  read_rds()


# ----------------------------------- Variables -------------------------------- #
metric_vars <- c('rh25', 'rh50', 'rh75', 'rh98',
                 're_below10m', 'aVDR', 'cover', 'pai', 'fhd_normal')


# ---------------------------- Extract fixed effects --------------------------- #
extract_effects <- function(model, metric_name) {
  broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE, conf.level = 0.95) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      metric = metric_name,
      effect = case_when(
        str_detect(term, ":")           ~ "Interaction",
        str_starts(term, "treatment")   ~ "Burned",
        str_starts(term, "time")        ~ "Timing",
        TRUE                             ~ term
      ),
      effect = factor(effect, levels = c("Burned", "Timing", "Interaction")),
      significant = p.value <= 0.01
    ) %>%
    select(metric, effect, estimate, std.error, conf.low, conf.high, p.value, significant)
}


# ----------------------- Fit one metric end-to-end ---------------------------- #
fit_one_metric <- function(var_name) {
  message("Fitting: ", var_name)
  
  pl <- convert_pairs2long(pairs2use, var = var_name, zscore = TRUE, downsample = TRUE,
                           time1 = '0', time2 = '1', treatment_col = 'zone')
  
  asp <- lme(value ~ treatment * time, data = pl,
             random = ~ 1 | index,
             control = lmeControl(opt = "optim"))
  
  sp  <- update(asp, correlation = corLin(form = ~ x + y, nugget = FALSE))
  
  extract_effects(sp, metric_name = var_name)
}


# ----------------------------- Loop over metrics ------------------------------ #
all_effects <- map_dfr(metric_vars, ~{
  tryCatch(fit_one_metric(.x),
           error = function(e) {
             message("  Skipped ", .x, ": ", e$message)
             NULL
           })
})

# Lock in the metric ordering for the plot (top-to-bottom matches metric_vars order)
all_effects <- all_effects %>%
  mutate(metric = factor(metric, levels = rev(metric_vars)))


# --------------------------------- Point-range plot --------------------------- #
ggplot(all_effects, aes(x = estimate, y = metric, color = effect)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.6),
                  size = 0.4, linewidth = 0.7) +
  scale_color_manual(values = c("Burned"      = "#E69F00",
                                "Timing"      = "#009E73",
                                "Interaction" = "#56B4E9")) +
  labs(x = "Normalized effect (95% CI)",
       y = NULL,
       color = NULL,
       title = "GEDI structural change: burned vs. control",
       subtitle = "Spatial mixed-effects model, z-scored response") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank())