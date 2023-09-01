## This script does some initial L0 cleaning of the master data (EXO and CSense)
## The low-hanging fruit are:
### 1. Data outside sensor bounds
### 2. Sensors were being maintained

# 1. Setup ---------------------------------------------------------------------

## Load constants
source("scripts/0_constants.R")


df_v0 <- read_csv("data/master_sensor_data_v0.csv")


# 2. Scrub non-sensical values -------------------------------------------------

temp_min = 0
temp_max = 40
sal_min = 20
sal_max = 35
do_min = -0.01
do_max = 20
ph_min = 5
ph_max = 9
co2_min = 0
co2_max = 1000


## b in ex below
limits <- tibble(parameter = c("temp_c", "sal_psu", "do_mgl"),
                 min = c(temp_min, sal_min, do_min),
                 max = c(temp_max, sal_max, do_max))
  
flag_values_outside_range <- function(data, parameter, limits_data) {
  limits <- limits_data %>%
    filter(parameter == !!parameter)
  
  if (nrow(limits) == 0) {
    warning("No limits found for the given parameter.")
    return(data)
  }
  
  min_limit <- limits$min
  max_limit <- limits$max
  
  flag_col_name <- paste0("flag_", parameter)
  
  data <- data %>%
    mutate(!!flag_col_name := ifelse(.data[[parameter]] < min_limit | .data[[parameter]] > max_limit, "OOR", NA))
  
  return(data)
}

parameters_to_flag <- limits$parameter
df_v1 <- reduce(parameters_to_flag, ~ flag_values_outside_range(.x, .y, limits), .init = df_v0)

df_v1