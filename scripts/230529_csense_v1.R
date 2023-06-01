## This script is v1 for looking at C-Sense data collected for Task 2A
##
## 2023-05-29
## Peter Regier
##
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(tidyverse, 
       parsedate,
       plotly,
       cowplot)

theme_set(theme_bw())


# 2. Read data -----------------------------------------------------------------

csense_raw <- read_delim("data/csense/timeseries/CR6Series_Table1.dat", skip = 1) %>% 
  slice(3:n()) %>% 
  mutate(datetime = parsedate::parse_date(TIMESTAMP), 
         co2_ppm_bare = as.numeric(SEVolt_1), 
         co2_ppm_eelgrass = as.numeric(SEVolt_2)) %>% 
  select(datetime, contains("co2_ppm"))

csense_long <- csense_raw %>% 
  pivot_longer(cols = contains("co2_ppm"))

p1 <- ggplot(csense_long, aes(datetime, value, color = name)) + 
  geom_line()

ggplotly(p1)

