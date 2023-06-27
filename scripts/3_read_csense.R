## This script brings in C-Sense data collected on a CR6. Important info: 
## SEVolt1 is in the Bare tank (Tank #5)
## SEVolt2 is in the Eelgrass tank (Tank #3)
##
## 2023-06-20
## Peter Regier
##
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, # keep things tidy
       parsedate, # parse_date()
       janitor, # clean_names()
       lubridate) # tidy datetime handling

## Location of raw EXO datasets
raw_filepath = "data/csense/timeseries"

## Load constants
source("scripts/0_constants.R")


# 2. Read in data --------------------------------------------------------------

## Currently, there should only be one datalogger file. If things get unwieldy,
## that might change, and we may need a list.files %>% map() path like exo

## Set threshold for flagging CO2 based on standard deviation
co2_sd_threshold = 50 #if the sd > threshold, cull

## Helper function
mean_ <- function(var){mean({{var}}, na.rm = T)}
sd_ <- function(var){sd({{var}}, na.rm = T)}

files <- list.files(raw_filepath, pattern = "CR6Series_Table1", full.names = T)

read_cr6 <- function(file){
  df_raw <- read_delim(file, skip = 1) %>% 
    slice(3:n()) %>% 
    mutate(SEVolt_1 = as.numeric(SEVolt_1), 
           SEVolt_2 = as.numeric(SEVolt_2)) %>% 
    mutate(datetime_1min = force_tz(parsedate::parse_date(TIMESTAMP), tzone = common_tz)) %>% 
    mutate(datetime = round_date(datetime_1min, unit = time_interval)) %>% 
    select(datetime, contains("SEVolt_"))  %>% 
    group_by(datetime) %>% 
    summarize(co2_ppm_bare = mean_(SEVolt_1), 
              co2_ppm_eelgrass = mean_(SEVolt_2),
              sd_ppm_bare = sd_(SEVolt_1), 
              sd_ppm_eelgrass = sd_(SEVolt_2)) 
}

df_raw <- files %>% 
  map(read_cr6) %>% 
  bind_rows()

df_raw <- read_delim(list.files(raw_filepath, pattern = "CR6Series_Table1", full.names = T), skip = 1) %>% 
  slice(3:n()) %>% 
    mutate(SEVolt_1 = as.numeric(SEVolt_1), 
         SEVolt_2 = as.numeric(SEVolt_2)) %>% 
  mutate(datetime_1min = force_tz(parsedate::parse_date(TIMESTAMP), tzone = common_tz)) %>% 
  mutate(datetime = round_date(datetime_1min, unit = time_interval)) %>% 
  select(datetime, contains("SEVolt_"))  %>% 
  group_by(datetime) %>% 
  summarize(co2_ppm_bare = mean_(SEVolt_1), 
            co2_ppm_eelgrass = mean_(SEVolt_2),
            sd_ppm_bare = sd_(SEVolt_1), 
            sd_ppm_eelgrass = sd_(SEVolt_2)) 


# 3. Make plots to explore data ------------------------------------------------

df_raw %>% 
  select(datetime, contains("co2")) %>% 
  pivot_longer(cols = -c(datetime)) %>% 
ggplot(aes(datetime, value, color = name)) + 
  geom_line()


### Some sort of stuff will need to go in the middle of this script, TBD depending
### on how EXO deployments are structured. But for now, leaving blank and exporting
### so it can be joined with pCO2 data.

write_csv(df_raw, "data/csense_all_data_raw.csv")

