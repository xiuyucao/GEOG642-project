# Convert GEDI pairs data from wide format to long format
# pairs is the GEDI pairs data I produced
# var is the variable name without time suffix (e.g., 'rh98')
convert_pairs2long <- function(pairs, var,
                               time1 = '0', time2 = '1',
                               treatment_col = "zone",
                               x_prefix = "x", y_prefix = "y"){
  var1 <- paste0(var, '_', time1)
  var2 <- paste0(var, '_', time2)
  x1 <- paste0(x_prefix, '_', time1)
  x2 <- paste0(x_prefix, '_', time2)
  y1 <- paste0(y_prefix, '_', time1)
  y2 <- paste0(y_prefix, '_', time2)
  
  required <- c(var1, var2, treatment_col, x1, x2, y1, y2)
  if (!all(required %in% names(pairs))){
    stop("Missing one or more required columns: ", 
         paste(setdiff(required, names(pairs)), collapse = ", "))
  }

  pairs_long <- pairs %>%
    st_drop_geometry() %>%
    mutate(index = row_number()) %>%
    select(index, all_of(c(var1, var2, x1, x2, y1, y2, treatment_col))) %>%
    # Pivot value, x, and y together so each row has matching coords
    pivot_longer(cols = -c(index, all_of(treatment_col)),
                 names_to = c(".value", "time"), names_pattern = paste0("(.+)_(", time1, "|", time2, ")$")) %>%
    rename(value = all_of(var)) %>%
    mutate(time = factor(time, levels = c(time1, time2)),
           treatment = factor(.data[[treatment_col]])) %>%
    select(index, value, time, treatment, x, y)
  
  pairs_long
}
