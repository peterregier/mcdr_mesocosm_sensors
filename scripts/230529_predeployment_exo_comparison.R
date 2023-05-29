
require(pacman)
p_load(tidyverse, 
       janitor, 
       parsedate)

df1 <- read_csv("/Users/regi350/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/peterregier/mcdr_mesocosm_sensors/data/exo/timeseries/230526_17H104984_predeploy_test.csv", 
                skip = 8) %>% clean_names() %>% 
  mutate(datetime = parsedate::parse_date(paste(date_mm_dd_yyyy, time_hh_mm_ss))) %>% 
  mutate(sensor = "17H")

df2 <- read_csv("/Users/regi350/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/peterregier/mcdr_mesocosm_sensors/data/exo/timeseries/230526_21B103964_predeploy_test.csv", 
                skip = 8) %>% clean_names() %>% 
  mutate(datetime = parsedate::parse_date(paste(date_mm_dd_yyyy, time_hh_mm_ss))) %>% 
  mutate(sensor = "21B")

df <- bind_rows(df1, df2)

df_wide <- inner_join(df1, df2, by = "datetime")


make_plot <- function(var1, var2){
  ggplot(df_wide, aes(x = datetime)) + 
    geom_point(aes(y = {{var1}}), color = "blue") + 
    geom_line(aes(y = {{var2}}), color = "gray")
}

make_plot(p_h_23, p_h_22.x)

ggplot(df1, aes(p_h_22, p_h_23)) + 
  geom_point()

ggplot(df2, aes(p_h_21, p_h_22)) + 
  geom_point()

ggplot(df, aes(datetime)) + 
  geom_line(aes(y = p_h_22), color = "blue") + 
  geom_line(aes(y = p_h_23), color = "gray")



