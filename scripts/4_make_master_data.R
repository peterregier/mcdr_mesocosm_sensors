## This script merges datasets to create a master dataset
##
## 2023-06-29
## Peter Regier
##
# ########## #
# ########## #

# 1. Setup ---------------------------------------------------------------------

## Load constants
source("scripts/0_constants.R")


# 2. Remake input datasets -----------------------------------------------------

## Remake datasets
source("scripts/1_read_exo_data.R")
source("scripts/3_read_csense.R")


# 3. Read in sensor datasets ---------------------------------------------------

csense_raw <- read_csv("data/csense_timeseries_raw.csv")
exo_raw <- read_csv("data/exo_timeseries_raw.csv") %>% 
  select(-c(contains("_v"), path))

## Create two dataframes, one for surface, one for deep
csense_bare <- csense_raw %>% 
  select(datetime_pdt, contains("_bare")) %>% ## Select just surface columns + metadata
  rename_with(~str_remove(., '_bare')) %>% ## Remove _surface so columns will match
  mutate(tank = "Bare")

csense_eelgrass <- csense_raw %>% 
  select(datetime_pdt, contains("_eelgrass")) %>% ## Select just surface columns + metadata
  rename_with(~str_remove(., '_eelgrass')) %>% ## Remove _surface so columns will match
  mutate(tank = "Eelgrass")

## Bind to a wide dataset
csense <- bind_rows(csense_bare, csense_eelgrass)

df <- full_join(exo_raw, csense, by = c("datetime_pdt", "tank")) %>% 
  mutate(time = as_hms(datetime_pdt))

master <- df %>% 
  mutate(datetime_pdt = as.character(datetime_pdt))

write_csv(master, "data/master_sensor_data_v0.csv")






