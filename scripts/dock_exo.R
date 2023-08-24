#Makes time series and daily average plots of the dock data and the tank exo data
#Finn Roach 

require(pacman)
p_load(tidyverse,
       janitor,
       lubridate,
       gridExtra,
       plotly,
       hms)

#read in the dock data and convert to pdt 
CTD <- read_delim("data/MCRLdata_CTD_20230529-20230807.csv")  %>%
  clean_names()
CTD <- CTD %>% mutate(time_utc = as.POSIXct(time_utc, tz = "GMT", tryFormats = c("%m/%d/%Y %H:%M:%OS",
                                                                     "%m/%d/%z %H:%M",
                                                                     "%m/%d/%Y %H:%M",
                                                                     "%m-%d-%z %H:%M")))
CTD <- CTD %>% mutate(time_utc = with_tz(time_utc, tz = "America/Los_Angeles"))
CTD <- CTD %>% rename("time_pdt" = "time_utc")
CTD <- CTD %>% mutate(time = format(as.POSIXct(time_pdt), format = "%H:%M:%S")) 
CTD <- CTD %>% mutate(time = as_hms(time))
#remove weird blip 
CTD[12910,] <- NA
view(CTD)
exodata <- read_delim("data/exo_timeseries_raw.csv")
exodata <- exodata %>% mutate(p_havg = rowMeans(exodata[, c('p_h1', 'p_h2')]),
                              datetime_pdt = force_tz(datetime_pdt, tz = "America/Los_Angeles")) 
#select the relevant columns and pivot so that there are different columns for the measurements from each tank
exodata <- exodata %>% select(datetime_pdt, temp_c, sal_psu, do_mgl, p_havg, p_h1, p_h2, tank)
exodata <- exodata %>% pivot_wider(names_from = tank, names_sep = "_", values_from = c(temp_c, sal_psu, do_mgl, p_havg, p_h1, p_h2))
exodata <- exodata %>% mutate(time = format(as.POSIXct(datetime_pdt), format = "%H:%M:%S")) 
exodata <- exodata %>% mutate(time = as_hms(time))
#cut out the recent bit of strange salinity data 
#exodata$sal_psu_Bare[8500:nrow(exodata)] <- NA

#code to check pH sensors, one was acting up recently
#startph <- "2023-08-08 00:00"
#endph <- "2023-08-21 00:00"
#ph_check <- exodata[exodata$datetime_pdt>startph&exodata$datetime_pdt<endph,]
#p_ph_check <- ggplot() + 
  #geom_line(data = ph_check, aes(datetime_pdt, p_h1_Bare, linetype = "1")) + 
  #geom_line(data = ph_check, aes(datetime_pdt, p_h2_Bare, linetype = "2")) +
  #labs(x = "Date", y = "pH", title = "pH")
#ggsave("ph_check_august_bare.png", p_ph_check)
#ggplotly(p_ph_check)

#CORRECTIONS
#offset for 5/29 to 7/25 based on calibration pre-clean checks  
eel_sal_offset <-2.08
bare_sal_offset <- 2.07
#for eelgrass sonde pH2 preformed better on the pre-clean checks during the 7/25 calibration
#construct column with pH2 up until this point and pHavg after for eelgrass 
#for bare sonde pH1` preformed better on the pre-clean checks during the 7/25 calibration
#construct column with pH1 up until this point and pHavg after for bare
exodata <- exodata[order(exodata$datetime_pdt),]
exodata <- exodata %>% mutate(ph_eel = c(p_h2_Eelgrass[1:16096], p_havg_Eelgrass[16097:nrow(exodata)]),
                              ph_bare = c(p_h1_Bare[1:16096], p_havg_Bare[16097:nrow(exodata)]),
                              sal_psu_Bare = c(sal_psu_Bare[1:16644] + bare_sal_offset, sal_psu_Bare[16645:nrow(exodata)]),
                              sal_psu_Eelgrass = c(sal_psu_Eelgrass[1:16644] + eel_sal_offset, sal_psu_Eelgrass[16645:nrow(exodata)])) %>% 
  select(-p_havg_Bare, -p_havg_Eelgrass, -p_h1_Bare, -p_h1_Eelgrass, -p_h2_Bare, -p_h2_Eelgrass)

#remove weird salinity data
exodata$sal_psu_Bare[8568:12708] <- NA
exodata$sal_psu_Bare[15524:16088] <- NA

#remove exo service dates
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

#start and end dates for these plots: 
start <- "2023-05-29 15:25:00"
end <- "2023-08-07 08:30:00"
exodata <- exodata[exodata$datetime_pdt>start&exodata$datetime_pdt<end,]
CTD <- CTD[CTD$time_pdt>start&CTD$time_pdt<end,]

s <- ggplot() + 
  geom_line(data = exodata, aes(datetime_pdt, sal_psu_Bare, color = "Bare"))+
  geom_line(data = exodata, aes(datetime_pdt, sal_psu_Eelgrass, color = "Eelgrass"))+
  geom_line(data = CTD, aes(time_pdt, salinity_ppt, color = "Dock")) + 
  labs(title="Salinity timeseries", x="Time", y="Salinity (psu)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = "none")+
  scale_color_manual(values=c("royalblue","maroon","forestgreen")) + ylim(28,32)
#ggsave("salinity_timeseries.png", height = 4, width = 16)
ggplotly(s)

t <- ggplot() + 
  geom_line(data = exodata, aes(datetime_pdt, temp_c_Bare, color = "Bare"))+
  geom_line(data = exodata, aes(datetime_pdt, temp_c_Eelgrass, color = "Eelgrass"))+
  geom_line(data = CTD, aes(time_pdt, temp_deg_c, color = "Dock")) + 
  labs(title="Temperature timeseries", x="Time", y="Temperature (C)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = "none")+
  scale_color_manual(values=c("royalblue","maroon","forestgreen"))
#ggsave("temp_timeseries.png", height = 4, width = 16)

do <- ggplot() + 
  geom_line(data = exodata, aes(datetime_pdt, do_mgl_Bare, color = "Bare"))+
  geom_line(data = exodata, aes(datetime_pdt, do_mgl_Eelgrass, color = "Eelgrass"))+
  geom_line(data = CTD, aes(time_pdt, do_mg_l, color = "Dock")) + 
  labs(title="Dissolved oxygen timeseries", x="Time", y="DO (mg/L)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = "none")+
  scale_color_manual(values=c("royalblue","maroon","forestgreen")) + ylim(0,14)
ggplotly(do)
#ggsave("do_timeseries.png", height = 4, width = 16)


ph <- ggplot() + 
  geom_line(data = exodata, aes(datetime_pdt, do_mgl_Bare, color = "Bare"))+
  geom_line(data = exodata, aes(datetime_pdt, do_mgl_Eelgrass, color = "Eelgrass"))+
  labs(title="pH timeseries", x="Time", y="pH") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("royalblue","forestgreen"))

#create dataframes for the daily average plots and graph them 
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
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("royalblue","maroon","forestgreen")) +
  ylim(10.8,16.5)
#ggsave("temp_daily.png", height = 4, width = 4)
ggplotly(t_avg)

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
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("royalblue","maroon","forestgreen"))+
  ylim(4,12.7)

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
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("royalblue","maroon","forestgreen")) +
  ylim(25,35)
#ggsave("salinity_daily.png",s_avg, height = 4, width = 6)


exoph_bare <- exodata %>% 
  select(time,ph_bare) %>% 
  group_by(time) %>% 
  summarize(mean = mean(ph_bare, na.rm = T), 
            sd = sd(ph_bare, na.rm = T))

exoph_eel <- exodata %>% 
  select(time,ph_eel) %>% 
  group_by(time) %>% 
  summarize(mean = mean(ph_eel, na.rm = T), 
            sd = sd(ph_eel, na.rm = T))

ph_avg <- ggplot(data = exoph_bare, aes(time,mean, color = "Bare")) + 
  geom_smooth(data = exoph_eel, aes(time, mean, color = "Eelgrass")) + 
  geom_errorbar(data = exoph_bare, aes(ymin = mean - sd, ymax = mean + sd, color = "Bare"), alpha = 0.2) +
  geom_errorbar(data = exoph_eel, aes(ymin = mean - sd, ymax = mean + sd, color = "Eelgrass"), alpha = 0.2) +
  geom_smooth() + labs(title="Mean daily pH", x="Time", y="Mean pH)") + theme_set(theme_bw()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("royalblue","maroon","forestgreen"))
#ggsave("salinity_daily.png",s_avg, height = 4, width = 6)

sal_plot<- grid.arrange(s,s_avg, widths = c(1, 0.5))
temp_plot <- grid.arrange(t,t_avg, widths = c(1, 0.5))
do_plot <- grid.arrange(do,do_avg, widths = c(1, 0.5))
ph_plot <- grid.arrange(ph,ph_avg, widths = c(1, 0.5))
ggsave("ph_exo.png",ph_plot, height = 4, width = 14)

#plot everything on one grid 
dock_exo <- grid.arrange(temp_plot, do_plot, sal_plot, nrow = 3)
ggsave("dock_exo.png",dock_exo, height = 10, width = 14)
temp_plot <- grid.arrange(t,t_avg, widths = c(0.8, 0.5))
do_plot <- grid.arrange(do,do_avg, widths = c(0.8, 0.5))
ggsave("do.png",do_plot, height = 4, width = 12)
ggsave("temp.png",temp_plot, height = 4, width = 12)

#print the maxes of these graphs 
print(CTDtemp$time[which.max(CTDtemp$mean)])
print(exotemp_bare$time[which.max(exotemp_bare$mean)])
print(exotemp_eel$time[which.max(exotemp_eel$mean)])
print(CTDdo$time[which.max(CTDdo$mean)])
print(exodo_bare$time[which.max(exodo_bare$mean)])
print(exodo_eel$time[which.max(exodo_eel$mean)])
print(CTDsal$time[which.max(CTDsal$mean)])
print(exosal_bare$time[which.max(exosal_bare$mean)])
print(exosal_eel$time[which.max(exosal_eel$mean)])
