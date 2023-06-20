## This script reads in data from EXO water quality sondes and exports a cleaned,
## formatted dataset for analysis
##
##
## NOTE: this script is built on test data from FY22 deployments and may need to
## be updated if data storage or export formats change
##
## pjr, 2023-04-11
##
# ############## #
# ############## #

## To DO
### 


# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, # keep things tidy
       parsedate, # parse_date()
       janitor, # clean_names()
       tictoc, # how long will this take...
       lubridate) # tidy datetime handling

## Set ggplot theme
theme_set(theme_bw())

## Location of raw EXO datasets
raw_filepath = "data/test/exo"

## Load constants
source("scripts/0_constants.R")


# 2. EXO functions -------------------------------------------------------------

files <- tibble(files = paste0(raw_filepath, "/", list.files(raw_filepath)))


encoding_vector <- list()
for(i in 1:nrow(files)){
  encoding_vector[[i]] <- guess_encoding(read_file_raw(files$files[i]))$encoding[1]
}


## This function checks that all files are in UTF-8
check_encoding <- function(directory){
  
  ## Create a tibble of all the files
  files <- tibble(files = paste0(directory, "/", list.files(directory)))
  
  encoding_vector <- list()
  for(i in 1:nrow(files)){
    encoding_vector[[i]] <- guess_encoding(read_file_raw(files$files[i]))$encoding[1]
  }
  

  ## This should always be empty
  wrong_encoding <- files %>% mutate(encoding = unlist(encoding_vector)) %>% 
    filter(encoding != "UTF-8")
  
  ## If it's not, throw a warning!
  #ifelse(nrow(wrong_encoding) > 0) warning(paste0("YOU HAVE ENCODING ISSUES: "), print(wrong_encoding$files), "all UTF-8")
  
  print("Files with non-UTF8 encoding (should be empty)")
  print(wrong_encoding)
}


## This function reads in EXO2 data (consistently in the eelgrass tank)
read_exo <- function(path) {
  read_csv(path, skip = 8) %>% 
    clean_names() %>% 
    mutate(datetime_raw = parsedate::parse_date(paste(date_mm_dd_yyyy, 
                                                      time_hh_mm_ss))) %>% 
    mutate(datetime = force_tz(datetime_raw, tzone = "America/Los_Angeles"),
           path = path) %>%
    rename("do_mgl" = "odo_mg_l",
           "ph" = "p_h") %>% 
    select(datetime, temp_c, depth_m, sal_psu, do_mgl, ph, battery_v, wiper_position_volt, path)
}


# 2. Read in raw datasets ------------------------------------------------------

## First check file formats (script needs UTF-8)
check_encoding(raw_filepath)

## Make a list of files to read in
exo_files <- list.files(raw_filepath, full.names = T)

df_raw <- exo_files %>% 
  map(read_exo) %>% 
  bind_rows()


# 3. Bin datasets to common time interval

tic("bin EXO data")
df_bin <- df_raw %>% 
  mutate(datetime = round_date(datetime, unit = time_interval)) %>% 
  group_by(datetime, path) %>% 
  summarize(across(where(is.numeric), mean, na.rm = T))
toc()

### Some sort of stuff will need to go in the middle of this script, TBD depending
### on how EXO deployments are structured. But for now, leaving blank and exorting
### so it can be joined with pCO2 data.


write_csv(df_bin, "data/test/exo_all_data_raw.csv")


