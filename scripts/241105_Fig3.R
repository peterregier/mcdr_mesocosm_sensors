## This script remakes Figure 3 based on Finn's "dock_exo.R" script. I take most
## of Finn's code verbatim
##
## Peter Regier
## 2024-11-05
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(tidyverse,
       janitor,
       lubridate,
       gridExtra,
       plotly,
       cowplot,
       readxl,
       hms)


# 2. Read in CTD ---------------------------------------------------------------

#read in the dock data and convert to pdt 
ctd_raw <- read_delim("data/MCRLdata_CTD_20230529-20230807.csv")  %>%
  clean_names() %>% 
  mutate(time_utc = as.POSIXct(time_utc, tz = "GMT", tryFormats = c("%m/%d/%Y %H:%M:%OS",
                                                                                 "%m/%d/%z %H:%M",
                                                                                 "%m/%d/%Y %H:%M",
                                                                                 "%m-%d-%z %H:%M"))) %>% 
  mutate(datetime_pdt = with_tz(time_utc, tz = "America/Los_Angeles")) %>% 
  #mutate(time = format(as.POSIXct(time_pdt), format = "%H:%M:%S")) %>% 
  mutate(time = as_hms(datetime_pdt))


# 3. Read in EXO ---------------------------------------------------------------

exo_raw <- read_delim("data/exo_timeseries_raw.csv") %>% 
  mutate(ph_avg = mean(c(p_h1, p_h2)), 
         datetime_pdt = force_tz(datetime_pdt, tz = "America/Los_Angeles")) %>% 
  dplyr::select(datetime_pdt, temp_c, sal_psu, do_mgl, ph_avg, p_h1, p_h2, tank) %>% 
  pivot_wider(names_from = tank, names_sep = "_", values_from = c(temp_c, sal_psu, do_mgl, ph_avg, p_h1, p_h2)) %>% 
  mutate(time_pdt = format(as.POSIXct(datetime_pdt), format = "%H:%M:%S")) %>% 
  mutate(time_pdt = as_hms(time_pdt)) 


# 4. Clean CTD -----------------------------------------------------------------

ctd_qc <- ctd_raw %>% 
  filter(qc_temp == 0 & 
           qc_do == 0 & 
           qc_salinity == 0) %>% 
  dplyr::select(-c(contains("qc_"))) %>% 
  mutate(datetime_pdt = round_date(datetime_pdt, "15 min")) %>% 
  group_by(datetime_pdt) %>% 
  summarize(temp_deg_c = mean(temp_deg_c, na.rm = T), 
            do_mg_l = mean(do_mg_l, na.rm = T), 
            salinity_ppt = mean(salinity_ppt, na.rm = T))

## Bring in daily spot checks to get salinity data for comparison
spot_checks <- read_xlsx("data/Daily_TankSpotChecks_2023.xlsx", skip = 1) %>% 
  clean_names() %>% 
  rename("datetime" = x1, 
         "sal_psu" = bottom_6) %>% 
  filter(x2 == "Inlet") %>% 
  mutate(date = as_date(datetime)) %>% 
  select(date, datetime, sal_psu)

## I don't like this
p1 <- ggplot() + 
  geom_line(data = ctd_qc, aes(datetime_pdt, salinity_ppt), color = "gray") + 
  geom_point(data = spot_checks, aes(datetime, sal_psu)) + 
  ggtitle("initial data comparison: dock (gray), inlet (black)")

compare_salinities <- ctd_qc %>% 
  mutate(date = as_date(datetime_pdt)) %>% 
  group_by(date) %>% 
  summarize(sal_psu_bay = mean(salinity_ppt, na.rm = T)) %>% 
  inner_join(spot_checks %>% filter(sal_psu < 31) %>% filter(date < "2023-08-05")) 

compare_salinities %>% 
  ggplot(aes(sal_psu_bay, sal_psu)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)

# intercept = -13.4, slope = 1.383
summary(lm(sal_psu~sal_psu_bay, data = compare_salinities))

ctd_cor <- ctd_qc %>% 
  mutate(sal_cor = (1.3 * salinity_ppt) - 10.7)

# 
p2 <- ggplot() + 
  geom_line(data = ctd_cor, aes(datetime_pdt, sal_cor), color = "gray") + 
  geom_point(data = compare_salinities, aes(datetime, sal_psu)) + 
  ggtitle("Offset data comparison: dock (gray), inlet (black)")

plot_grid(p1, p2, ncol = 1)
ggsave("figures/241210_compare_salinities.png", width = 6, height = 7)

# 4. First visualization -------------------------------------------------------

plot_data <- function(data, var){
  ggplot(data, aes(datetime_pdt, {{var}})) + geom_line()
}

## Temp
plot_grid(plot_data(ctd_qc, temp_deg_c), 
          plot_data(exo_raw, temp_c_Bare), 
          plot_data(exo_raw, temp_c_Eelgrass), 
          ncol = 1)

## DO
plot_grid(plot_data(ctd_qc, do_mg_l), 
          plot_data(exo_raw, do_mgl_Bare), 
          plot_data(exo_raw, do_mgl_Eelgrass), 
          ncol = 1)

## Sal
x1 <- exo_raw %>% 
  filter(sal_psu_Bare > 28 | is.na(sal_psu_Bare)) %>% 
  filter(sal_psu_Eelgrass > 10 | is.na(sal_psu_Eelgrass)) 

plot_grid(plot_data(ctd_qc, salinity_ppt) + geom_hline(yintercept = 30, color = "gray"), 
          plot_data(x1, sal_psu_Bare) + geom_hline(yintercept = 30, color = "gray"), 
          plot_data(x1, sal_psu_Eelgrass) + geom_hline(yintercept = 30, color = "gray"), 
          ncol = 1)


# 5. Finn's EXO cleaning -------------------------------------------------------

## Rename to feed into Finn's workflow
exodata <- exo_raw

#CORRECTIONS
#offset for 5/29 to 7/25 based on calibration pre-clean checks  
eel_sal_offset <-2.08
bare_sal_offset <- 2.07
#for eelgrass sonde pH2 preformed better on the pre-clean checks during the 7/25 calibration
#construct column with pH2 up until this point and pHavg after for eelgrass 
#for bare sonde pH1` preformed better on the pre-clean checks during the 7/25 calibration
#construct column with pH1 up until this point and pHavg after for bare
exodata <- exodata[order(exodata$datetime_pdt),]
exodata <- exodata %>% mutate(ph_eel = c(p_h2_Eelgrass[1:16096], ph_avg_Eelgrass[16097:nrow(exodata)]),
                              ph_bare = c(p_h1_Bare[1:16096], ph_avg_Bare[16097:nrow(exodata)]),
                              sal_psu_Bare = c(sal_psu_Bare[1:16644] + bare_sal_offset, sal_psu_Bare[16645:nrow(exodata)]),
                              sal_psu_Eelgrass = c(sal_psu_Eelgrass[1:16644] + eel_sal_offset, sal_psu_Eelgrass[16645:nrow(exodata)])) %>% 
  select(-ph_avg_Bare, -ph_avg_Eelgrass, -p_h1_Bare, -p_h1_Eelgrass, -p_h2_Bare, -p_h2_Eelgrass)

#remove weird salinity data
exodata$sal_psu_Bare[8568:12708] <- NA
exodata$sal_psu_Bare[15524:16088] <- NA

#remove exo service dates. I'm not rewriting because I don't have time, but I do 
## NOT like leaving this code in. Sorry to whoever is reading this!
start1 <- "2023-06-12 13:00:00"
end1 <- "2023-06-12 16:00:00"
start2 <- "2023-06-26 12:00:00"
end2 <- "2023-06-26 15:00:00"
start3 <- "2023-07-11 10:00"
end3 <- "2023-07-12 10:00"
start4 <- "2023-07-24 14:00"
end4 <- "2023-07-28 12:00"
start5 <- "2023-08-07 14:00"
end5 <- "2023-08-07 17:00"
start6 <- "2023-08-21 10:00"
end6 <- "2023-08-21 13:00"
exodata[exodata$datetime_pdt>start1&exodata$datetime_pdt<end1,2:ncol(exodata)] <- NA
exodata[exodata$datetime_pdt>start2&exodata$datetime_pdt<end2,2:ncol(exodata)] <- NA
exodata[exodata$datetime_pdt>start3&exodata$datetime_pdt<end3,2:ncol(exodata)] <- NA
exodata[exodata$datetime_pdt>start4&exodata$datetime_pdt<end4,2:ncol(exodata)] <- NA
exodata[exodata$datetime_pdt>start5&exodata$datetime_pdt<end5,2:ncol(exodata)] <- NA
exodata[exodata$datetime_pdt>start6&exodata$datetime_pdt<end6,2:ncol(exodata)] <- NA


# 6. EXO v2 visualization ------------------------------------------------------

## Temp - I think good to go
plot_grid(plot_data(exodata, temp_c_Bare), 
          plot_data(exodata, temp_c_Eelgrass), 
          ncol = 1)

## DO - good to go
plot_grid(plot_data(exodata, do_mgl_Bare), 
          plot_data(exodata, do_mgl_Eelgrass), 
          ncol = 1)

## Sal - needs help
plot_grid(plot_data(exodata, sal_psu_Bare) + geom_hline(yintercept = 30, color = "gray"), 
          plot_data(exodata, sal_psu_Eelgrass) + geom_hline(yintercept = 30, color = "gray"), 
          ncol = 1)


# 7. Clean EXO salinity --------------------------------------------------------

exo_clean <- exodata %>% 
  filter(sal_psu_Bare > 28 | is.na(sal_psu_Bare)) %>% 
  filter(sal_psu_Eelgrass > 10 | is.na(sal_psu_Eelgrass)) 

ggplot() +
  geom_line(data = exodata, aes(datetime_pdt, sal_psu_Bare, color = "Eelgrass")) +
  geom_line(data = exodata, aes(datetime_pdt, sal_psu_Eelgrass, color = "Bare"))

ggplot() +
  geom_line(data = exo_clean, aes(datetime_pdt, sal_psu_Bare, color = "Eelgrass")) +
  geom_line(data = exo_clean, aes(datetime_pdt, sal_psu_Eelgrass, color = "Bare"))

#x <- anti_join(exodata, exo_clean, by = "datetime_pdt")


# 8. Combine datasets ----------------------------------------------------------

#start and end dates for these plots: 
start <- "2023-05-29 15:25:00"
end <- "2023-08-07 08:30:00"

exo_trim <- exo_clean %>% 
  filter(datetime_pdt > start & 
           datetime_pdt < end) %>% 
  mutate(datetime_pdt = round_date(datetime_pdt, "15 min")) %>% 
  group_by(datetime_pdt) %>% 
  summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

ctd_trim <- ctd_cor %>% 
  filter(datetime_pdt > start & 
           datetime_pdt < end)

ctd_trim %>% 
  filter(month(datetime_pdt) == 7) %>% 
  mutate(tod = as_hms(datetime_pdt), 
         date = as_date(datetime_pdt)) %>% 
  group_by(tod) %>% 
  ggplot() + 
  geom_line(aes(tod, do_mg_l, group = date))
  
df <- full_join(exo_trim, ctd_trim, by = "datetime_pdt")


# 4. First visualization -------------------------------------------------------

plot_data_v2 <- function(data, dock_var, bare_var, eel_var, y_lab){
  ggplot() + 
    geom_line(data = data, aes(datetime_pdt, {{dock_var}}, color = "Dock"), show.legend = F) +
    geom_line(data = data, aes(datetime_pdt, {{bare_var}}, color = "Bare"), show.legend = F) +
    geom_line(data = data, aes(datetime_pdt, {{eel_var}}, color = "Eelgrass"), show.legend = F) +
    theme_set(theme_bw()) + 
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    scale_color_manual(values=c("royalblue", "maroon","forestgreen")) + 
    labs(x = "Datetime", y = y_lab, color = "")
}

p_temp <- plot_data_v2(df, temp_deg_c, temp_c_Bare, temp_c_Eelgrass, "Temp (C)")
p_sal <- plot_data_v2(df, salinity_ppt-1, sal_psu_Bare, sal_psu_Eelgrass, "Salinity (PSU)")
p_do <- plot_data_v2(df, do_mg_l, do_mgl_Bare, do_mgl_Eelgrass, "DO (mg/L)")

plot_ts <- plot_grid(p_temp, p_sal, p_do, ncol = 1, labels = c("a", "b", "c"))


plot_data_v3 <- function(dock_var, bare_var, eel_var){
  
  x <- df %>% 
    mutate(tod = as_hms(datetime_pdt)) %>% 
    group_by(tod) %>% 
    summarize(dock_mean = mean({{dock_var}}, na.rm = T), 
              dock_sd = sd({{dock_var}}, na.rm = T), 
              bare_mean = mean({{bare_var}}, na.rm = T), 
              bare_sd = sd({{bare_var}}, na.rm = T), 
              eel_mean = mean({{eel_var}}, na.rm = T), 
              eel_sd = sd({{eel_var}}, na.rm = T))
  
  print(paste("Dock:", range(x$dock_mean)))
  print(paste("Bare:", range(x$bare_mean)))
  print(paste("Eel:", range(x$eel_mean)))
  
  ggplot() +
    geom_smooth(data = x, aes(tod, dock_mean, color = "Dock"), se = F) +
    geom_errorbar(data = x, aes(tod, ymin = dock_mean - dock_sd, ymax = dock_mean + dock_sd, color = "Dock"), alpha = 0.2) + 
    geom_smooth(data = x, aes(tod, bare_mean, color = "Bare"), se = F) +
    geom_errorbar(data = x, aes(tod, ymin = bare_mean - bare_sd, ymax = bare_mean + bare_sd, color = "Bare"), alpha = 0.2) + 
    geom_smooth(data = x, aes(tod, eel_mean, color = "Eelgrass"), se = F) +
    geom_errorbar(data = x, aes(tod, ymin = eel_mean - eel_sd, ymax = eel_mean + eel_sd, color = "Eelgrass"), alpha = 0.2) + 
    theme_set(theme_bw()) +
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    scale_color_manual(values=c("royalblue","maroon","forestgreen")) + 
    labs(x = "Datetime", y = "", color = "")
  

  

  
}

plot_daily <- plot_grid(plot_data_v3(temp_deg_c, temp_c_Bare, temp_c_Eelgrass), 
                        plot_data_v3(sal_cor, sal_psu_Bare, sal_psu_Eelgrass), 
          plot_data_v3(do_mg_l, do_mgl_Bare, do_mgl_Eelgrass), 
          ncol = 1)

## Temp ranges: 
### Dock: 12.2-13.1
### Bare: 12.6-15.0
### Eel: 13.1-15.4

## Sal ranges: 
### Dock: 29.6-29.7
### Bare: 29.8-29.9
### Eel: 30.0-30.1

## DO ranges: 
### Dock: 6.8-8.4
### Bare: 7.1-10.0
### Eel: 6.7-11.4

plot_grid(plot_ts, plot_daily, nrow = 1, rel_widths = c(1, 0.4))
ggsave("figures/241210_Figure3.png", width = 12, height = 8)
          















