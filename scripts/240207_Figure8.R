## This script creates Figure 8, which is a series of time-series plots
## Source data created by 231210_prep_timeseries.Rmd and then CO2 corrected by
## 
## Variables to include: Temp, pH, DO, salinity, CO2
##
## 2024-02-07
## Peter Regier
##
# ########### #

source("scripts/0_constants.R")

plot_start <- "2023-08-04"
plot_end <- "2023-08-21"

df <- read_csv("data/240207_timeseries_corrected.csv") %>% 
  filter(datetime > plot_start & 
           datetime < plot_end) %>% 
  drop_na()

seacarb_output <- carb(flag = 21, 
                       var1 = df$co2_ppm_calc, 
                       var2 = df$p_h2, 
                       T = df$temp_c, 
                       S = df$sal_psu) %>% 
  mutate(datetime = df$datetime, 
         dic_mol_L = DIC * 1.025) %>% # convert kg to L (1.025 kg/L for seawater)
  as_tibble() %>% 
  clean_names()

## This might be right? Not sure. But: 
# (mol CaCO3/kg seawater) * (~100g CaCO3 / 1 mol CaCO3) * 
# (1000 mg / g) * (1.03 kg / L seawater)
## 
# https://www.ultimatereef.net/pages/alkalinity_converter/: 50 ppm = 1 meq/L
# 1 meq/L = 1000 meq/m3
df_combined <- df %>% 
  bind_cols(seacarb_output %>% select(alk, omega_aragonite)) %>% 
  mutate(alk_mol_kg = alk) %>% 
  mutate(alk_umol_kg = alk * 1000000) %>% #we're assuming this is mol of charge, per pg 7 of https://www.soest.hawaii.edu/oceanography/courses/OCN623/Spring2012/CO2pH.pdf
  mutate(alk_meq_kg = alk_mol_kg) %>% #unnecessary, keep for clarity
  mutate(alk_meq_m3 = alk_meq_kg * (1.03*1000)) #(meq/kg)*(1.03kg/L seawater)*(1000L/m3)
# mutate(alk_ppm_caco3 = alk * 100 * 1000 * 1.03) %>% 
# mutate(alk_meqm3 = alk_ppm_caco3 * (1000/50))

make_plot <- function(var, resolution, y_label){
  
  ## Set scaling for geom_rect and matching axes
  rect_start <- as_datetime("2023-08-07 10:00:00")
  y_min = min(df_combined %>% drop_na() %>% pull({{var}}))
  y_max = max(df_combined %>% drop_na() %>% pull({{var}}))
  alpha_level = 0.4
  
  p1 <- ggplot(df_combined, aes(datetime, {{var}}, color = tank)) + 
    geom_ribbon(aes(ymin = {{var}} - resolution, ymax = {{var}} + resolution, fill = tank), 
                color = NA, alpha = alpha_level, show.legend = F) + 
    geom_line(show.legend = F) + 
    annotate(geom = "rect", 
             xmin = rect_start,
             xmax = as_datetime("2023-08-07 20:00:00"), 
             ymin = y_min, 
             ymax = y_max, 
             alpha = 0.2) + 
    annotate(geom = "text", 
             x = rect_start + hours(36), 
             y = y_max, 
             label = "Maintenance", 
             size = 2) + 
    labs(x = "", y = y_label) + 
    scale_color_manual(values=c("royalblue","forestgreen")) + 
    scale_fill_manual(values=c("royalblue","forestgreen"))
  
  p2 <- df_combined %>%
    mutate(tod = hour(datetime)) %>%
    group_by(tank, tod) %>%
    summarize(sd = sd({{var}}, na.rm = T),
              #max = max({{var}}, na.rm = T),
              mean = mean({{var}}, na.rm = T)) %>%
    ggplot(aes(tod, mean, color = tank)) +
    geom_ribbon(aes(ymin = mean - resolution, ymax = mean + resolution, fill = tank), 
                color = NA, alpha = alpha_level, show.legend = F) + 
    geom_errorbar(aes(ymin = mean - sd, 
                      ymax = mean + sd), 
                  alpha = 0.5) + 
    geom_point() + 
    labs(x = "Time of day", 
         y = y_label, 
         color = "") + 
    scale_color_manual(values=c("royalblue","forestgreen")) + 
    scale_fill_manual(values=c("royalblue","forestgreen"))
  
  plot_grid(p1, p2, nrow = 1, rel_widths = c(1, 0.7))
}

plot_grid(make_plot(temp_c, 0.2, "Temp. (C)"), #Accuracy is 0.2 C per EXO manual
          make_plot(p_h2, 0.2, "pH (NBS)"), #Accuracy is 0.2 units per EXO manual
          make_plot(do_mgl, 0.1, "DO (mg/L)"), #Accuracy is 0.1 mg/L per EXO manual
          make_plot(co2_ppm_calc, 0.03*2000, "CO2 (ppm)"), #Based on accuracy of 3% of full scale
          make_plot(alk_umol_kg, 0.1, "Alk (umol/kg)"), 
          ncol = 1)
ggsave("figures/240207_Fig8_v1.png", width = 10, height = 12)



