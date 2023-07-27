## This is the start of a script to look at bucket-tests. It's getting far enough
## to look at the post-cal bucket tests and compare them, but won't go farther today
##

# 1. Setup ---------------------------------------------------------------------

## Location of raw EXO datasets
bucket_directory = "data/exo/bucket_tests"

## Load constants
source("scripts/0_constants.R")


# 2. Check encoding is UTF-8 for all files prior to importing data -------------

## Check file formats (script needs UTF-8) - should be empty!
check_encoding(bucket_directory) 


# 3. Read in EXO time-series ---------------------------------------------------

## The function read_exo() is now defined in the constants script because it will
## be used for both exo time-series and the bucket tests

bucket_files <- tibble(file = list.files(path = bucket_directory, full.names = T)) 

df_bare <- bucket_files %>% 
  filter(grepl("bare", file)) %>% 
  pull() %>% 
  map(read_exo) %>% 
  bind_rows() %>% 
  rename("p_h1" = p_h_22, "p_h2" = p_h_23)

df_eelgrass <- bucket_files %>% 
  filter(grepl("eelgrass", file)) %>% 
  pull() %>% 
  map(read_exo) %>% 
  bind_rows() %>% 
  rename("p_h1" = p_h_21, "p_h2" = p_h_22)

df_bucket <- bind_rows(df_bare %>% mutate(tank = "Bare"), 
                df_eelgrass %>% mutate(tank = "Eelgrass")) 

comparison_plot <- function(var){
  df_bucket %>% 
    filter(datetime > "2023-07-25" & sal_psu > 10) %>% 
    ggplot(aes(datetime, {{var}}, color = tank)) + 
    geom_line()
} 

plot_grid(comparison_plot(sal_psu), 
          comparison_plot(p_h1), 
          comparison_plot(p_h2), 
          comparison_plot(do_perc))


