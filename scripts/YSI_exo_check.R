require(pacman)
p_load(tidyverse, 
       parsedate,
       janitor,
       readxl,
       hms,
       lubridate,
       data.table,
       zoo,
       gridExtra)

#Code to read in YSI data adapted from kay 
### Read in the data from Excel
ysidata <- read_excel("data/Daily_TankSpotChecks_2023.xlsx")

### Select rows and columns, rename columns, select columns for df 
ysidata <- ysidata[2:164,] %>% ### CHANGE AS MORE DATA IS ADDED TO EXCEL
  rename("sample_id" = "Sample ID", "t_surface" = "T (C)", "t_deep" = "...4", "salinity_surface" = "Salinity (ppt)", "salinity_deep" = "...6", "pH_surface" = "pH (NBS)", "pH_deep" = "...8", "do_surface" = "DO (mg/L)", "do_deep" = "...10", "time" = "Time Start(PSD)", "date" = "Date") %>%
  select(-Notes,-Initials,-`Calibration (y/n)`, -`Time Stop(PSD)`, -`Flow Rate (L/min)`) 

### Convert chars to numeric from data frame, change sample id to eelgrass densities

ysidata <- ysidata %>% mutate_at(c('t_surface', 't_deep', 'salinity_surface', 'salinity_deep', 'pH_surface', 'pH_deep', 'do_surface','do_deep'), as.numeric) %>%
  drop_na(date)
 
ysidata$time <- na.locf(ysidata$time) 

#for some reason the times get saved with a random date attached, reformat to lose the date
ysidata$time <- format(ymd_hms(ysidata$time), "%H:%M:%S")


ysidata <- ysidata %>% filter(grepl(paste(c(3,5), collapse='|'), sample_id)) %>%
  mutate(datetime = as.POSIXct(paste(parse_date(date), time))) 

ysidata <- ysidata %>% select(sample_id, datetime, t_surface, salinity_surface, pH_surface, do_surface) %>%
  mutate(datetime = force_tz(datetime, tz = "America/Los_Angeles"))

ysidata <- ysidata %>% pivot_wider(names_from = sample_id, names_sep = "_", values_from = c(t_surface, salinity_surface, pH_surface, do_surface))

view(ysidata)

exodata <- read_delim("data/exo_timeseries_raw.csv") 
exodata <- exodata %>% mutate(p_havg = rowMeans(exodata[, c('p_h1', 'p_h2')]),
                              datetime_pdt = force_tz(datetime_pdt, tz = "America/Los_Angeles")) 
exodata <- exodata %>% select(datetime_pdt, temp_c, sal_psu, do_mgl, p_havg, tank)
exodata <- exodata %>% pivot_wider(names_from = tank, names_sep = "_", values_from = c(temp_c, sal_psu, do_mgl, p_havg))

view(exodata)

#converts data frames to data tables
ysidata <- data.table(ysidata)
exodata <- data.table(exodata)   

#sets keys to align timecodes 
setkey(ysidata, "datetime")
setkey(exodata, "datetime_pdt")

#aligns the sensor and grab sample data by nearest timecode 
aligned <- exodata[ysidata, roll = TRUE]

view(aligned)

ph_bare <- ggplot(na.omit(aligned), aes(p_havg_Bare, pH_surface_5))+ 
  geom_point()+
  geom_smooth(method="lm",color="blue")+
  labs(title="pH", x="pH", y="pH") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))
summary(lm(p_havg_Bare~pH_surface_5, data=na.omit(aligned)))

t_bare <- ggplot(na.omit(aligned), aes(temp_c_Bare, t_surface_5))+ 
  geom_point()+
  geom_smooth(method="lm",color="blue")+
  labs(title="Temperature", x="Temperature (C)", y="Temperature (C)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))
summary(lm(temp_c_Bare~t_surface_5, data = na.omit(aligned)))

do_bare <- ggplot(na.omit(aligned), aes(do_mgl_Bare, do_surface_5))+ 
  geom_point()+
  geom_smooth(method="lm",color="blue")+
  labs(title="DO", x="DO(mg/L)", y="DO (mg/L)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))
summary(lm(do_mgl_Bare~do_surface_5, data = na.omit(aligned)))

s_bare <- ggplot(na.omit(aligned), aes(sal_psu_Bare,salinity_surface_5))+ 
  geom_point()+
  geom_smooth(method="lm",color="blue")+
  labs(title="Salinity ", x="Salinity (psu)", y="Salinity (psu)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))
summary(lm(sal_psu_Bare~salinity_surface_5, data = na.omit(aligned)))

p_bare <- grid.arrange(ph_bare, t_bare, do_bare, s_bare,nrow = 2, top = "Zero Density Eelgrass", left = "Handheld YSI", bottom = "Exo sensor")


ggsave("bare_exo_YSI.png",p_bare, height = 6, width = 6)

ph_eel <- ggplot(na.omit(aligned), aes(p_havg_Eelgrass, pH_surface_3))+ 
  geom_point()+
  geom_smooth(method="lm",color="darkgreen")+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color="darkgreen")+
  labs(title="pH", x="pH", y="pH") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))
summary(lm(p_havg_Eelgrass~pH_surface_3, data=na.omit(aligned)))

t_eel <- ggplot(na.omit(aligned), aes(temp_c_Eelgrass, t_surface_3))+ 
  geom_point()+
  geom_smooth(method="lm",color="darkgreen")+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color="darkgreen")+
  labs(title="Temperature", x="Temperature (C)", y="Temperature (C)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))
summary(lm(temp_c_Eelgrass~t_surface_3, data = na.omit(aligned)))

do_eel <- ggplot(na.omit(aligned), aes(do_mgl_Eelgrass, do_surface_3))+ 
  geom_point()+
  geom_smooth(method="lm",color="darkgreen")+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color="darkgreen")+
  labs(title="DO", x="DO(mg/L)", y="DO (mg/L)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))
summary(lm(do_mgl_Eelgrass~do_surface_3, data = na.omit(aligned)))

s_eel <- ggplot(na.omit(aligned), aes(sal_psu_Eelgrass,salinity_surface_3))+ 
  geom_point()+
  geom_smooth(method="lm",color="darkgreen")+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color="darkgreen")+
  labs(title="Salinity ", x="Salinity (psu)", y="Salinity (psu)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  ylim(27.3, 29.8)
summary(lm(sal_psu_Eelgrass~salinity_surface_3, data = na.omit(aligned)))

p_eel <- grid.arrange(ph_eel, t_eel, do_eel, s_eel,nrow = 2, top = "High Density Eelgrass", left = "Handheld YSI", bottom = "Exo sensor")


ggsave("eelgrass_exo_YSI.png",p_eel, height = 6, width = 6)
