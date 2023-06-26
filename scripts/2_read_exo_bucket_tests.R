## This script reads in bucket tests conducted with the EXOs, formats them,
## cleans them, then constructs comparison plots to compare sensor performance
## and ID potential issues
## 
## 2023-06-20
## Peter Regier
##
# ########### #
# ########### #

## Load packages
require(pacman)
p_load(tidyverse, # keep things tidy
       parsedate, # parse_date()
       janitor, # clean_names()
       tictoc, # how long will this take...
       lubridate) # tidy datetime handling

## Location of raw EXO datasets
bucket_directory = "data/exo/bucket_tests"

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
check_encoding(bucket_directory) 




