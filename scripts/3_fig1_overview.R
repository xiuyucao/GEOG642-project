# Major fires to study in CA, 2020
library(sf)
library(ggplot2)


## --------------------------------- File Paths -------------------------------- ##
ca_boundary_path <- "data/raw/boundaries/ca.shp"
fires_path <- "data/input/fires/ca_2020_fires.shp"


## -------------------------------- Read Layers -------------------------------- ##
ca_boundary <- st_read(ca_boundary_path)
fire_boundaries <- st_read(fires_path)


## -------------------------------- Plot Figure -------------------------------- ##
fig <- ggplot() +
  geom_sf(data = ca_boundary, fill = "grey98", color = "grey25", linewidth = 0.5) +
  geom_sf(data = fire_boundaries,
          aes(fill = Incid_Name),
          color = "#8c2d04",
          alpha = 0.55,
          linewidth = 0.35) +
  labs(fill = "") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_line(color = "grey88", linewidth = 0.2),
        axis.title = element_blank(),
        legend.position = "bottom")

ggsave("figures/fig1_overview.png", fig, width = 8, height = 8)
