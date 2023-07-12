## This script creates plots to visualize time-series, diurnal patterns, and 
## multivariate relationships of sensors. 
## Eventually, I'd like to add in grab samples
##
## 2023-06-29
## Peter Regier
##
# ########## #
# ########## #

# 1. Setup ---------------------------------------------------------------------

## Load constants
source("scripts/0_constants.R")

## If you haven't recently rebuilt the master dataset, do so
source("scripts/4_make_master_data.R")


# 2. Import data ---------------------------------------------------------------

data <- read_csv("data/master_sensor_data.csv") %>% 
  filter(sal_psu > 20)


x <- read_csv("data/master_sensor_data.csv") %>% 
  filter(tank == "Bare") %>% 
  filter(datetime_pdt > "2023-07-01") %>% 
  ggplot(aes(datetime_pdt, sal_psu)) + 
  geom_line()

p_load(plotly)
ggplotly(x)


# 3. Make time-series plots ----------------------------------------------------

make_ts_plot <- function(var, y_label){
  
  p0 <- ggplot(data, aes(datetime_pdt, {{var}}, color = tank)) + 
  geom_line() + 
  labs(x = "", y = y_label, color = "") + 
    theme(legend.position = "none")
  
  p1 <- data %>% 
    select(time, tank, {{var}}) %>% 
    group_by(time, tank) %>% 
    summarize(mean = mean({{var}}, na.rm = T), 
              sd = sd({{var}}, na.rm = T)) %>% 
    ggplot(aes(time, mean, color = tank)) + 
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2) +
    geom_smooth()
  
  plot_grid(p0, p1, rel_widths = c(1, 0.4))
}


temp_plot <- make_ts_plot(temp_c, "Temp (C)")
sal_plot <- make_ts_plot(sal_psu, "Salinity")
co2_plot <- make_ts_plot(co2_ppm, "CO2 (ppm)")
do_plot <- make_ts_plot(do_mgl, "DO (mg/L)")


# 4. Make pH time-series plots -------------------------------------------------

df_ph <- data %>% 
  select(datetime_pdt, time, tank, contains("p_h")) %>% 
  pivot_longer(cols = -c(datetime_pdt, time, tank)) %>% 
  mutate(name = case_when(name == "p_h1" ~ "Sensor 1", 
                          name == "p_h2" ~ "Sensor 2"))
  
ph0 <- ggplot(df_ph, aes(datetime_pdt, value, color = tank, linetype = name)) + 
  geom_line() + 
  labs(x = "", y = "pH", color = "") + 
  theme(legend.position = "none")

ph1 <- df_ph %>% 
  select(time, tank, value) %>% 
  group_by(time, tank) %>% 
  summarize(mean = mean(value, na.rm = T), 
            sd = sd(value, na.rm = T)) %>% 
  ggplot(aes(time, mean, color = tank)) + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2) +
  geom_smooth()

ph_plot <- plot_grid(ph0, ph1, rel_widths = c(1, 0.4))


# 5. Final time-series plot ----------------------------------------------------

plot_grid(temp_plot, 
          sal_plot,
          co2_plot,
          do_plot, 
          ph_plot,
          ncol = 1)
ggsave("figures/timeseries_check.png", width = 10, height = 12)
 


# X. Import and format maintenance intervals -----------------------------------

sensor_check_times <- read_excel("data/from_sharedrive/Bi-weekly_Tank_Sampling_2023.xlsx", sheet = 3) %>% 
  slice(3:n()) %>% 
  clean_names()


