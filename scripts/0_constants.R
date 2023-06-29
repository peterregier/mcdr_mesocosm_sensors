## This script holds constants that keep things consistent between different 
## scripts

## Load packages
require(pacman)
p_load(tidyverse, # keep things tidy
       parsedate, # parse_date()
       janitor, # clean_names()
       hms,
       lubridate) # tidy datetime handling

## Set the temporal bin-rate for both datasets
time_interval = "5 min"

## Set common tz to use across all datetimes to PDT (not sure why, but it's + 
## instead of - to get PDT)
common_tz = "Etc/GMT+7"

## Set ggplot theme
theme_set(theme_bw())

