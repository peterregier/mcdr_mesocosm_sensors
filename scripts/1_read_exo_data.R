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

## Location of raw EXO datasets
ts_directory = "data/exo/timeseries"

## Load constants
source("scripts/0_constants.R")


# 2. Check encoding is UTF-8 for all files prior to importing data -------------

## Check file formats (script needs UTF-8) - should be empty!
check_encoding(ts_directory) 


# 3. Read in EXO time-series ---------------------------------------------------

## The function read_exo() is now defined in the constants script because it will
## be used for both exo time-series and the bucket tests

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

plot_variable(do_perc) + geom_hline(yintercept = 100)

# Write out data 

df_final <- df %>% 
  mutate(datetime_pdt = as.character(datetime)) %>% 
  relocate(datetime_pdt) %>% 
  select(-datetime) 

write_csv(df_final, "data/exo_timeseries_raw.csv")



