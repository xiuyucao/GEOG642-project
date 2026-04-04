library(tidyverse)
library(RSQLite)
library(sf)

## -------------------------------- Global Vars -------------------------------- ##
fiadb_file <- 'data/raw/fia-ca-2026-04-02/SQLite_FIADB_CA.db'
sql_evalid2query_file <- 'scripts/query_evalid2query.sql'
sql_query_plots_file_measyear <- 'scripts/query_plots.sql'
sql_query_plots_file_evalid <- 'scripts/query_plots_evalid.sql'
sql_query_trees_file <- 'scripts/query_trees.sql'
out_folder <- 'data/input/fia'


# function to write the plots to .shp for quick preview
write_plots_shp <- function(plots, output_dir='data/temp', suffix='') {
  plots_sf <- plots %>%
    select(PLT_CN, LAT, LON, MEASYEAR) %>%
    st_as_sf(coords = c("LON", "LAT"), crs = 4269)
  
  # Write to .shp
  st_write(plots_sf, file.path(output_dir, paste0("plots_", suffix, ".shp")), delete_dsn = TRUE)
}


## ------------------------------ If Use MEASYEAR ------------------------------ ##
# Get plots
con <- dbConnect(SQLite(), fiadb_file)
plots <- dbGetQuery(con, read_file(sql_query_plots_file_measyear)) %>%
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


## ------------------------------- If Use EVALID ------------------------------- ##
# # Get EVALID for CA (6)
# con <- dbConnect(SQLite(), fiadb_file)
# evaluation <- dbGetQuery(con, 
#                          "select * from POP_EVAL 
#                          where STATECD = 6 and EVALID like '%01'") %>%
#   arrange(EVALID)
# dbDisconnect(con)
# 
# # Write EVALID and descriptions to a text file
# out_lines <- apply(evaluation, 1, function(row) {
#   paste(row["EVALID"], " --", row["EVAL_DESCR"], sep = " ")})
# 
# # Add commas after EVALID except for the last line
# out_lines <- ifelse(seq_along(out_lines) < length(out_lines),
#                     gsub("  --", ",  --", out_lines), out_lines)
# writeLines(out_lines, 'scripts/evalid2query.txt')

# Get plots
con <- dbConnect(SQLite(), fiadb_file)
plots <- dbGetQuery(con, read_file(sql_query_plots_file_evalid)) %>%
  mutate(meas_date = make_date(MEASYEAR, MEASMON, 1))
dbDisconnect(con)

# Write results
p_2020 <- plots %>%
  filter(EVALID==62001)
write_plots_shp(p_2020, suffix='2020')
write_rds(p_2020, file.path(out_folder, 'plots_eval20.rds'), compress='xz')

p_2021 <- plots %>%
  filter(EVALID==62101)
write_plots_shp(p_2021, suffix='2021')
write_rds(p_2021, file.path(out_folder, 'plots_eval21.rds'), compress='xz')


## --------------------------------- Get Trees --------------------------------- ##
con <- dbConnect(SQLite(), fiadb_file)
trees <- dbGetQuery(con, read_file(sql_query_trees_file))
dbDisconnect(con)

write_rds(trees, file.path(out_folder, 'trees.rds'), compress='xz')
