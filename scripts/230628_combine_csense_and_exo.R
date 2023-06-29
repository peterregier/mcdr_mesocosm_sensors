## This script brings C-Sense and EXO data together to create a master sensor dataset
##
## 2023-06-28
## Peter Regier
##
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## Load constants
source("scripts/0_constants.R")


# 2. Read in data and merge ----------------------------------------------------

exo <- read_csv("data/exo_timeseries_raw.csv")

csense_raw <- read_csv("data/csense_all_data_raw.csv")
  
## Create two dataframes, one for surface, one for deep
csense_bare <- csense_raw %>% 
  select(datetime, contains("_bare")) %>% ## Select just surface columns + metadata
  rename_with(~str_remove(., '_bare')) %>% ## Remove _surface so columns will match
  mutate(tank = "Bare")

csense_eelgrass <- csense_raw %>% 
  select(datetime, contains("_eelgrass")) %>% ## Select just surface columns + metadata
  rename_with(~str_remove(., '_eelgrass')) %>% ## Remove _surface so columns will match
  mutate(tank = "Eelgrass")

csense <- bind_rows(csense_bare, csense_eelgrass)

df <- full_join(exo, csense, by = c("datetime", "tank")) %>% 
  mutate(time = as_hms(datetime))


ggplot(df, aes(do_mgl, co2_ppm, color = time)) + geom_point() + 
  facet_wrap(~tank, nrow = 1, scales = "free")


