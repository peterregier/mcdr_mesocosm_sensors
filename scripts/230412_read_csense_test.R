## This script reads in data from the C-Sense CO2 sensors logged via CR6 datalogger, 
##and exports a cleaned, formatted dataset for analysis
##
## NOTE: this script is built on test data from FY22 deployments from a different 
## CO2 sensors, and may need to be updated if data storage or export formats change
##
## pjr, 2023-04-12
##
# ############## #
# ############## #


# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, # keep things tidy
       parsedate, # parse_date()
       janitor, # clean_names()
       lubridate) # tidy datetime handling

## Set ggplot theme
theme_set(theme_bw())

## Location of raw EXO datasets
raw_filepath = "data/test/csense"

## Load constants
source("scripts/constants.R")

# 2. Read in data --------------------------------------------------------------

## Currently, there should only be one datalogger file. If things get unwieldy,
## that might change, and we may need a list.files %>% map() path like exo

## Set threshold for flagging CO2 based on standard deviation
co2_sd_threshold = 50 #if the sd > threshold, cull

df_raw <- read_delim(list.files(raw_filepath, full.names = T), skip = 1) %>% 
  slice(3:n()) %>% 
  mutate(datetime_1min = force_tz(parsedate::parse_date(TIMESTAMP), tzone = "America/Los_Angeles")) %>% 
  mutate(datetime = round_date(datetime_1min, unit = time_interval)) %>% 
  filter(STDEVA < co2_sd_threshold & STDEVB < co2_sd_threshold) %>% # first, filter out high standard deviations
  filter(ACO2AVE > 0 & BCO2AVE > 0) %>% # second, filter out negative or 0 readings
  group_by(datetime) %>% # now, bin into 5 minute intervals to match exo intervals
  summarize(ACO2AVE = mean(ACO2AVE), 
            BCO2AVE = mean(BCO2AVE),
            STDEVA = mean(STDEVA),
            STDEVB = mean(STDEVB)) %>%
  select(datetime, ACO2AVE, BCO2AVE, STDEVA, STDEVB) 


### Some sort of stuff will need to go in the middle of this script, TBD depending
### on how EXO deployments are structured. But for now, leaving blank and exorting
### so it can be joined with pCO2 data.

write_csv(df_raw, "data/test/csense_all_data_raw.csv")
