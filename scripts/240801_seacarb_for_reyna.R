## Script for Reyna to use seacarb. I'll make it as straightforward as possible.
## The most important thing with seacarb is checking the units of variables you're
## putting in, and looking up the units of variables you're exporting. R help and
## the package documents are your friends here
##
## 2024-08-01
## Peter Regier

## Load packages using pacman
require(pacman)

p_load(tidyverse, 
       readxl,
       seacarb,
       ggpubr,
       cowplot,
       janitor)

## Set ggplot theme
theme_set(theme_bw())

## Ignore this, setting up a toy dataset for you
# df <- read_csv("data/240207_timeseries_corrected.csv") %>% 
#   drop_na() %>% 
#   filter(tank == "Eelgrass") %>% 
#   filter(datetime > as_datetime("2023-08-14 00:00:00")) %>% 
#   filter(datetime < as_datetime("2023-08-18 00:00:00")) %>% 
#   rename("ph" = p_h2) %>% 
#   dplyr::select(datetime, temp_c, sal_psu, ph, co2_ppm_calc)
# 
# write_csv(df, "data/240801_reyna.csv")

## Read in data
df <- read_csv("data/240801_reyna.csv")

## This is the meat of the script
## type ?carb into your console to get a description of the carb() function
seacarb_output <- carb(flag = 21, 
                       var1 = df$co2_ppm_calc, 
                       var2 = df$ph, 
                       T = df$temp_c, 
                       S = df$sal_psu) %>% 
  mutate(datetime = df$datetime) %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(alk_umol_kg = alk * 1e6, 
         dic_umol_kg = dic * 1e6) %>% 
  select(datetime, alk_umol_kg, dic_umol_kg, omega_aragonite)

## Combine datasets
df_final <- bind_cols(df, seacarb_output %>% select(-datetime))

## helper function to make graphs
make_plots <- function(var){
  ggplot(df_final, aes(datetime, {{var}})) + geom_line()
}

## Look at seacarb calculations
plot_grid(make_plots(ph), 
          make_plots(co2_ppm_calc),
          make_plots(alk_umol_kg),
          make_plots(dic_umol_kg),
          make_plots(omega_aragonite), 
          ncol = 1)




