## This script holds constants that keep things consistent between different 
## scripts

## Load packages
require(pacman)
p_load(tidyverse, # keep things tidy
       parsedate, # parse_date()
       janitor, # clean_names()
       hms,
       cowplot,
       readxl,
       lubridate) # tidy datetime handling

## Set the temporal bin-rate for both datasets
time_interval = "5 min"

## Set common tz to use across all datetimes to PDT (not sure why, but it's + 
## instead of - to get PDT)
common_tz = "Etc/GMT+7"

## Set ggplot theme
theme_set(theme_bw())

###### FUNCTIONS #########


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


## Set function to read in EXO2 raw datasets
read_exo <- function(path) {
  read_csv(path, skip = 8) %>% 
    clean_names() %>% 
    mutate(datetime_raw = parsedate::parse_date(paste(date_mm_dd_yyyy, 
                                                      time_hh_mm_ss))) %>% 
    mutate(datetime = force_tz(datetime_raw, tzone = common_tz),
           path = path) %>%
    mutate(datetime_raw = as.character(datetime_raw)) %>% 
    rename("do_mgl" = "odo_mg_l", 
           "do_perc" = odo_percent_local) %>% 
    select(datetime, datetime_raw, temp_c, depth_m, sal_psu, do_perc, do_mgl, contains("p_h"), battery_v, wiper_position_volt, path) %>% 
    select(-contains("_m_v"))
}
