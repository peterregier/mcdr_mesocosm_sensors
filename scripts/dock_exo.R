require(pacman)
p_load(tidyverse,
       janitor,
       lubridate,
       gridExtra)

CTD <- read_delim("data/CTD_20230701_dock.csv")  %>%
  clean_names()
CTD <- CTD %>% mutate(time_utc = with_tz(parse_date(time_utc), tz = "America/Los_Angeles"))
CTD <- CTD %>% rename("time_pdt" = "time_utc")

exodata <- read_delim("data/exo_timeseries_raw.csv")
exodata <- exodata %>% mutate(p_havg = rowMeans(exodata[, c('p_h1', 'p_h2')]),
                              datetime_pdt = force_tz(datetime_pdt, tz = "America/Los_Angeles")) 
#select the relevant columns and pivot so that there are different columns for the measurements from each tank
exodata <- exodata %>% select(datetime_pdt, temp_c, sal_psu, do_mgl, p_havg, tank)
exodata <- exodata %>% pivot_wider(names_from = tank, names_sep = "_", values_from = c(temp_c, sal_psu, do_mgl, p_havg))

exodata <- exodata %>% mutate(time = format(as.POSIXct(datetime_pdt), format = "%H:%M:%S")) 
exodata <- exodata %>% mutate(time = as_hms(time))

CTD <- CTD %>% mutate(time = format(as.POSIXct(time_pdt), format = "%H:%M:%S")) 
CTD <- CTD %>% mutate(time = as_hms(time))

exodata$sal_psu_Bare[8500:nrow(exodata)] <- NA


s <- ggplot() + 
  geom_line(data = exodata, aes(datetime_pdt, sal_psu_Bare, color = "Bare"))+
  geom_line(data = exodata, aes(datetime_pdt, sal_psu_Eelgrass, color = "Eelgrass"))+
  geom_line(data = CTD, aes(time_pdt, salinity_ppt, color = "Dock")) + 
  labs(title="Salinity timeseries", x="Time", y="Salinity (psu)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("blue","brown","green")) + ylim(26,32)
#ggsave("salinity_timeseries.png", height = 4, width = 16)

t <- ggplot() + 
  geom_line(data = exodata, aes(datetime_pdt, temp_c_Bare, color = "Bare"))+
  geom_line(data = exodata, aes(datetime_pdt, temp_c_Eelgrass, color = "Eelgrass"))+
  geom_line(data = CTD, aes(time_pdt, temp_deg_c, color = "Dock")) + 
  labs(title="Temperature timeseries", x="Time", y="Temperature (C)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("blue","brown","green"))
#ggsave("temp_timeseries.png", height = 4, width = 16)

do <- ggplot() + 
  geom_line(data = exodata, aes(datetime_pdt, do_mgl_Bare, color = "Bare"))+
  geom_line(data = exodata, aes(datetime_pdt, do_mgl_Eelgrass, color = "Eelgrass"))+
  geom_line(data = CTD, aes(time_pdt, do_mg_l, color = "Dock")) + 
  labs(title="Dissolved oxygen timeseries", x="Time", y="DO (mg/L)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("blue","brown","green"))
#ggsave("do_timeseries.png", height = 4, width = 16)

exotemp_bare <- exodata %>% 
  select(time,temp_c_Bare) %>% 
  group_by(time) %>% 
  summarize(mean = mean(temp_c_Bare, na.rm = T), 
            sd = sd(temp_c_Bare, na.rm = T))

exotemp_eel <- exodata %>% 
  select(time,temp_c_Eelgrass) %>% 
  group_by(time) %>% 
  summarize(mean = mean(temp_c_Eelgrass, na.rm = T), 
            sd = sd(temp_c_Eelgrass, na.rm = T))

CTDtemp <- CTD %>% 
  select(time,temp_deg_c) %>% 
  group_by(time) %>% 
  summarize(mean = mean(temp_deg_c, na.rm = T), 
            sd = sd(temp_deg_c, na.rm = T)) 

t_avg <- ggplot(data = exotemp_bare, aes(time,mean, color = "Bare")) + 
  geom_smooth(data = exotemp_eel, aes(time, mean, color = "Eelgrass")) + 
  geom_smooth(data = CTDtemp, aes(time, mean, color = "Dock")) + 
  geom_errorbar(data = exotemp_bare, aes(ymin = mean - sd, ymax = mean + sd, color = "Bare"), alpha = 0.2) +
  geom_errorbar(data = exotemp_eel, aes(ymin = mean - sd, ymax = mean + sd, color = "Eelgrass"), alpha = 0.2) +
  geom_errorbar(data = CTDtemp, aes(ymin = mean - sd, ymax = mean + sd, color = "Dock"), alpha = 0.2) +
  geom_smooth() + labs(title="Mean daily temperature", x="Time", y="Mean Temperature (C)") + theme_set(theme_bw()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("blue","brown","green"))
#ggsave("temp_daily.png", height = 4, width = 4)

exodo_bare <- exodata %>% 
  select(time,do_mgl_Bare) %>% 
  group_by(time) %>% 
  summarize(mean = mean(do_mgl_Bare, na.rm = T), 
            sd = sd(do_mgl_Bare, na.rm = T))

exodo_eel <- exodata %>% 
  select(time,do_mgl_Eelgrass) %>% 
  group_by(time) %>% 
  summarize(mean = mean(do_mgl_Eelgrass, na.rm = T), 
            sd = sd(do_mgl_Eelgrass, na.rm = T))

CTDdo <- CTD %>% 
  select(time,do_mg_l) %>% 
  group_by(time) %>% 
  summarize(mean = mean(do_mg_l, na.rm = T), 
            sd = sd(do_mg_l, na.rm = T)) 

do_avg <- ggplot(data = exodo_bare, aes(time,mean, color = "Bare")) + 
  geom_smooth(data = exodo_eel, aes(time, mean, color = "Eelgrass")) + 
  geom_smooth(data = CTDdo, aes(time, mean, color = "Dock")) + 
  geom_errorbar(data = exodo_bare, aes(ymin = mean - sd, ymax = mean + sd, color = "Bare"), alpha = 0.2) +
  geom_errorbar(data = exodo_eel, aes(ymin = mean - sd, ymax = mean + sd, color = "Eelgrass"), alpha = 0.2) +
  geom_errorbar(data = CTDdo, aes(ymin = mean - sd, ymax = mean + sd, color = "Dock"), alpha = 0.2) +
  geom_smooth() + labs(title="Mean daily dissolved oxygen", x="Time", y="Mean dissolved oxygen (mg/L)") + theme_set(theme_bw()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("blue","brown","green"))
#ggsave("do_daily.png", height = 4, width = 6)

exosal_bare <- exodata %>% 
  select(time,sal_psu_Bare) %>% 
  group_by(time) %>% 
  summarize(mean = mean(sal_psu_Bare, na.rm = T), 
            sd = sd(sal_psu_Bare, na.rm = T))

exosal_eel <- exodata %>% 
  select(time,sal_psu_Eelgrass) %>% 
  group_by(time) %>% 
  summarize(mean = mean(sal_psu_Eelgrass, na.rm = T), 
            sd = sd(sal_psu_Eelgrass, na.rm = T))

CTDsal <- CTD %>% 
  select(time,salinity_ppt) %>% 
  group_by(time) %>% 
  summarize(mean = mean(salinity_ppt, na.rm = T), 
            sd = sd(salinity_ppt, na.rm = T)) 

s_avg <- ggplot(data = exosal_bare, aes(time,mean, color = "Bare")) + 
  geom_smooth(data = exosal_eel, aes(time, mean, color = "Eelgrass")) + 
  geom_smooth(data = CTDsal, aes(time, mean, color = "Dock")) + 
  geom_errorbar(data = exosal_bare, aes(ymin = mean - sd, ymax = mean + sd, color = "Bare"), alpha = 0.2) +
  geom_errorbar(data = exosal_eel, aes(ymin = mean - sd, ymax = mean + sd, color = "Eelgrass"), alpha = 0.2) +
  geom_errorbar(data = CTDsal, aes(ymin = mean - sd, ymax = mean + sd, color = "Dock"), alpha = 0.2) +
  geom_smooth() + labs(title="Mean daily salinity", x="Time", y="Mean salinity (psu)") + theme_set(theme_bw()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("blue","brown","green"))
#ggsave("salinity_daily.png",s_avg, height = 4, width = 6)

sal_plot<- grid.arrange(s,s_avg, widths = c(1, 0.4))
temp_plot <- grid.arrange(t,t_avg, widths = c(1, 0.4))
do_plot <- grid.arrange(do,do_avg, widths = c(1, 0.4))
dock_exo <- grid.arrange(temp_plot, do_plot, sal_plot, nrow = 3)

ggsave("dock_exo.png",dock_exo, height = 10, width = 16)

print(CTDtemp$time[which.max(CTDtemp$mean)])
print(exotemp_bare$time[which.max(exotemp_bare$mean)])
print(exotemp_eel$time[which.max(exotemp_eel$mean)])

print(CTDdo$time[which.max(CTDdo$mean)])
print(exodo_bare$time[which.max(exodo_bare$mean)])
print(exodo_eel$time[which.max(exodo_eel$mean)])

print(CTDsal$time[which.max(CTDsal$mean)])
print(exosal_bare$time[which.max(exosal_bare$mean)])
print(exosal_eel$time[which.max(exosal_eel$mean)])
