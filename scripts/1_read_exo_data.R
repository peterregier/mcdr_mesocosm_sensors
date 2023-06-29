## This script is based off the now-archived 230411_read_exo_test.R script. The
## goal is to read in all EXO data from the Summer/Fall 2023 deployments in two
## mesocosm tanks. These files come in two flavors: 1) 5-minute time-series which
## are the actual dataset of interest, and 2) bucket tests at 5s intervals which
## we are using to assess how comparable the sondes are reading in an identical
## controlled environment (ie a bucket of seawater...). Additionally, code is 
## partially sourced from a previous script that did basically the same thing:
## https://github.com/peterregier/eed_ldrd_asv/blob/main/scripts/1_read_in_exo.R.
## NOTE: this script is written on Mac and may have unknown issues on a PC...
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
       tictoc, # how long will this take...
       lubridate) # tidy datetime handling

## Location of raw EXO datasets
ts_directory = "data/exo/timeseries"

## Load constants
source("scripts/0_constants.R")


# 2. Check encoding is UTF-8 for all files prior to importing data -------------

## This function checks that all files are in UTF-8
check_encoding <- function(directory){
  ## Create a tibble of all the files
  files <- tibble(files = paste0(directory, "/", list.files(directory)))
  
  encoding_vector <- list()
  for(i in 1:nrow(files)){
    encoding_vector[[i]] <- guess_encoding(read_file_raw(files$files[i]))$encoding[1]
  }
  
  wrong_encoding <- tibble(files = files, 
         encoding = unlist(encoding_vector)) %>% 
    filter(encoding != "UTF-8")

  ## If it's not, throw a warning!
  if(nrow(wrong_encoding) > 0) warning(paste0("YOU HAVE ENCODING ISSUES BROOOO: "), print(wrong_encoding$files))
}

## Check file formats (script needs UTF-8) - should be empty!
check_encoding(ts_directory) 


# 3. Read in EXO time-series ---------------------------------------------------

## Set function to read in EXO2 raw datasets
## This function reads in EXO2 data (consistently in the eelgrass tank)
read_exo <- function(path) {
  read_csv(path, skip = 8) %>% 
    clean_names() %>% 
    mutate(datetime_raw = parsedate::parse_date(paste(date_mm_dd_yyyy, 
                                                      time_hh_mm_ss))) %>% 
    mutate(datetime = force_tz(datetime_raw, tzone = "America/Los_Angeles"),
           path = path) %>%
    rename("do_mgl" = "odo_mg_l") %>% 
    select(datetime, temp_c, depth_m, sal_psu, do_mgl, contains("p_h"), battery_v, wiper_position_volt, path) %>% 
    select(-contains("_m_v"))
}

ts_files <- tibble(file = list.files(path = ts_directory, full.names = T)) %>% 
  slice(3:n())


df_bare <- ts_files %>% 
  filter(grepl("bare", file)) %>% 
  pull() %>% 
  map(read_exo) %>% 
  bind_rows() %>% 
  rename("p_h1" = p_h_22, "p_h2" = p_h_23)

df_eelgrass <- ts_files %>% 
  filter(grepl("eelgrass", file)) %>% 
  pull() %>% 
  map(read_exo) %>% 
  bind_rows() %>% 
  rename("p_h1" = p_h_21, "p_h2" = p_h_22)

df <- bind_rows(df_bare %>% mutate(tank = "Bare"), 
                df_eelgrass %>% mutate(tank = "Eelgrass"))


## Create exploratory plots ----------------------------------------------------

plot_variable <- function(var, y_label){
  ggplot(df, aes(datetime, {{var}}, color = tank)) + 
    geom_line()
}


write_csv(df, "data/exo_timeseries_raw.csv")



