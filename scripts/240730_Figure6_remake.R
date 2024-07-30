## This is yet another updated script for what used to be Figure 8 but is now
## Figure 6. We're going to remake this to include 
## 1. pH, 2. pCO2, 3. Alkalinity, 4. DIC, 4 Omega aragonite
##
## 2024-02-07
## Peter Regier
##
# ########### #

source("scripts/0_constants.R")

plot_start <- "2023-08-04"
plot_end <- "2023-08-21"

df <- read_csv("data/240207_timeseries_corrected.csv") %>% 
  filter(datetime > plot_start & 
           datetime < plot_end) %>% 
  drop_na()

