library(tidyverse)
library(RSQLite)
library(sf)

## -------------------------------- Global Vars -------------------------------- ##
fiadb_file <- 'data/raw/fia-ca-2026-04-02/SQLite_FIADB_CA.db'
sql_query_plots <- 'scripts/query_plots.sql'
sql_query_trees <- 'scripts/query_trees.sql'
out_folder <- 'data/input/fia'


# function to write the plots to .shp for quick preview
write_plots_shp <- function(plots, output_dir='data/temp', suffix='') {
  plots_sf <- plots %>%
    select(PLT_CN, LAT, LON, MEASYEAR) %>%
    st_as_sf(coords = c("LON", "LAT"), crs = 4269)
  
  # Write to .shp
  st_write(plots_sf, file.path(output_dir, paste0("plots_", suffix, ".shp")), delete_dsn = TRUE)
}


## ------------------------------ Query Database ------------------------------- ##
# Get plots
con <- dbConnect(SQLite(), fiadb_file)
plots <- dbGetQuery(con, read_file(sql_query_plots)) %>%
  mutate(meas_date = make_date(MEASYEAR, MEASMON, 1))
dbDisconnect(con)

# Write results
p_before <- plots %>%
  filter(meas_date >= as.Date("2014-08-01"), meas_date <  as.Date("2020-08-01"))
write_plots_shp(p_before, suffix='2014-2020')
write_rds(p_before, file.path(out_folder, 'plots_2014-2020.rds'), compress='xz')

p_after <- plots %>%
  filter(meas_date >= as.Date("2020-10-01"), meas_date <  as.Date("2026-08-01"))
write_plots_shp(p_after, suffix='2020-2026')
write_rds(p_after, file.path(out_folder, 'plots_2020-2026.rds'), compress='xz')


## --------------------------------- Get Trees --------------------------------- ##
con <- dbConnect(SQLite(), fiadb_file)
trees <- dbGetQuery(con, read_file(sql_query_trees))
dbDisconnect(con)

write_rds(trees, file.path(out_folder, 'trees.rds'), compress='xz')
