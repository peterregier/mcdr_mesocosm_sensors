require(pacman)
p_load(tidyverse,
       janitor,
       lubridate,
       gridExtra,
       plotly,
       readxl)

#Clean CTD data get time in correct format and timezone 
#read in the dock data and convert to pdt 
CTD <- read_delim("data/CTD_20230701_dock.csv")  %>%
  clean_names()
CTD <- CTD %>% mutate(time_utc = as.POSIXct(time_utc, tz = "GMT", tryFormats = c("%m/%d/%Y %H:%M:%OS",
                                                                                 "%m/%d/%z %H:%M",
                                                                                 "%m/%d/%Y %H:%M",
                                                                                 "%m-%d-%z %H:%M")))
CTD <- CTD %>% mutate(time_utc = with_tz(time_utc, tz = "America/Los_Angeles"))
CTD <- CTD %>% rename("time_pdt" = "time_utc")
CTD <- CTD %>% mutate(time = format(as.POSIXct(time_pdt), format = "%H:%M:%S")) 
CTD <- CTD %>% mutate(time = as_hms(time))
CTD <- CTD %>% mutate("date" = as.Date(trunc(time_pdt, 'days')))


#Clean exo data get time in correct format 
exodata <- read_delim("data/exo_timeseries_raw.csv")
exodata <- exodata %>% mutate(p_havg = rowMeans(exodata[, c('p_h1', 'p_h2')]),
                              datetime_pdt = force_tz(datetime_pdt, tz = "America/Los_Angeles")) 
#select the relevant columns and pivot so that there are different columns for the measurements from each tank
exodata <- exodata %>% select(datetime_pdt, temp_c, sal_psu, do_mgl, p_havg, tank)
exodata <- exodata %>% pivot_wider(names_from = tank, names_sep = "_", values_from = c(temp_c, sal_psu, do_mgl, p_havg))
exodata <- exodata %>% mutate(time = format(as.POSIXct(datetime_pdt), format = "%H:%M:%S")) 
exodata <- exodata %>% mutate(time = as_hms(time))
exodata <- exodata %>% mutate("date" = as.Date(trunc(datetime_pdt, 'days')))
#cut out the recent bit of strange salinity data 
exodata$sal_psu_Bare[8500:nrow(exodata)] <- NA
exodata <- exodata %>% select(-datetime_pdt, -p_havg_Eelgrass, -p_havg_Bare)
view(exodata)


#get daily mins and maxes and the times of those mins and maxes for temp and do in the bare tank 
exo_daily_bare <- exodata %>% 
  group_by(date) %>% 
  summarise(., across(.cols=c(temp_c_Bare, do_mgl_Bare), c(max = max, time_max = ~time[which.max(.x)], min = min, time_min = ~time[which.min(.x)]), .names = "{.fn}_{.col}"))

view(exo_daily_bare)

#get daily mins and maxes and the times of those mins and maxes for temp and do in the eelgrass tank 
exo_daily_eel <- exodata %>% 
  group_by(date) %>% 
  summarise(., across(.cols=c(temp_c_Eelgrass, do_mgl_Eelgrass), c(max = max, time_max = ~time[which.max(.x)], min = min, time_min = ~time[which.min(.x)]), .names = "{.fn}_{.col}"))

view(exo_daily_eel)

#get daily mins and maxes and the times of those mins and maxes for temp and do from the dock data  
CTD_daily <- CTD %>% 
  group_by(date) %>% 
  summarise(., across(.cols=c(temp_deg_c, do_mg_l), c(max = max, time_max = ~time[which.max(.x)], min = min, time_min = ~time[which.min(.x)]), .names = "{.fn}_{.col}"))

view(CTD_daily)

#merge datasets 
daily_bare <- merge(CTD_daily, exo_daily_bare, by = "date", all = TRUE)
view(daily_bare)
daily <- merge(daily_bare, exo_daily_eel, by = "date", all = TRUE)
view(daily)

#calculate lag time based on the max temp differences between dock and bare and dock and eelgrass in hours
lag_temp <- daily %>% mutate(bare_max = as.numeric(time_max_temp_c_Bare - time_max_temp_deg_c)/3600,
                        eel_max = as.numeric(time_max_temp_c_Eelgrass - time_max_temp_deg_c)/3600,
                        bare_min = as.numeric(time_min_temp_c_Bare - time_min_temp_deg_c)/3600,
                        eel_min = as.numeric(time_min_temp_c_Eelgrass - time_min_temp_deg_c)/3600) %>% select(bare_max, eel_max, eel_min, bare_min, date) 
lag_temp <- lag_temp %>% mutate(avg_eel = rowMeans(lag_temp[, c('eel_max','eel_min')]),
                                avg_bare = rowMeans(lag_temp[, c('bare_max', 'bare_min')]))
lag_temp <- lag_temp %>% select(eel_max, bare_max, date) %>% pivot_longer(c(-date),names_to = c("Var", ".value"), names_sep = "_")

#now lets calculate daily residence time 
#code to read in YSI data adapted from kay 
#read in the data from Excel
flow <- read_excel("data/Daily_TankSpotChecks_2023.xlsx") %>% rename("sample_id" = "Sample ID", "t_surface" = "T (C)", "t_deep" = "...4", "salinity_surface" = "Salinity (ppt)", "salinity_deep" = "...6", "pH_surface" = "pH (NBS)", "pH_deep" = "...8", "do_surface" = "DO (mg/L)", "do_deep" = "...10", "time" = "Time Start(PSD)", "date" = "Date", "old_flow" = "Old Flow Rate (L/min)", "new_flow" = "New Flow Rate (L/min)")
#convert chars to numeric from data frame, change sample id to eelgrass densities
flow <- flow %>% select(sample_id, old_flow, new_flow, do_surface, t_surface, date)  %>% drop_na(old_flow) %>% slice(2:nrow(flow))
#make sure timezone is right and select relevant columns
flow <- flow %>% mutate(sample_id = as.numeric(sample_id),
                        old_flow = as.numeric(old_flow),
                        new_flow = as.numeric(new_flow),
                        do_surface = as.numeric(do_surface),
                        t_surface = as.numeric(t_surface),
                        date = as.Date(date))
#shift old flow one day back so that the avg is the new flow set in morning on day 1 
#and the flow it changes to the next day (ie add an NA at the beginning of the new flow column so it looks like the flows taken on same day)
view(flow)
flow$old_flow <- c(flow$old_flow[2:nrow(flow)], NA)
flow <- flow %>% mutate(avg_flow = rowMeans(flow[,c('old_flow','new_flow')]))
view(flow)
flow <- flow %>% pivot_longer(c(-date, -sample_id, -do_surface, -t_surface, -avg_flow),
               names_to = c("Var", ".value"),
               names_sep = "_")
view(flow)
#read in the volume data from Excel
volume <- read_excel("data/ResidenceTime_eelgrasstanks.xlsx") %>% clean_names() %>% slice(1:8)
volume <- volume %>% rename("sample_id" = "tank") %>% select(sample_id, est_volume_l)
residence <- merge(flow, volume, by = "sample_id")
#create a residence time column 
#USE AVERAGE FLOW instead of flow
residence <- residence %>% mutate(restime = est_volume_l/avg_flow)
residence <- residence %>% mutate(restime_hours = restime/60)
residence <- residence %>% filter(grepl(paste(c(3,5), collapse='|'), sample_id)) 
resdence <- residence %>% filter(grepl("old", Var))
residence$sample_id[residence$sample_id == "3"] <- "eel" 
residence$sample_id[residence$sample_id == "5"] <- "bare" 
view(residence)
#plot residence time and max values for each tank 
ggplot() + 
  geom_line(data = na.omit(residence), aes(date, restime_hours, color = "Residence"), linewidth= 1)+
  geom_point(data = na.omit(residence), aes(date, restime_hours, color = "Residence"))+
  geom_line(data = na.omit(lag_temp), aes(date, max, color = "Lag"), linewidth= 1)+
  geom_point(data = na.omit(lag_temp), aes(date, max, color = "Lag"))+
  theme_set(theme_bw())+
  scale_color_manual(values=c("gold", "royalblue"))+
  facet_wrap(~sample_id, 
             ncol= 2)


ggsave("lag_temp_max_residence_series.png")

resnew <- residence %>% filter(grepl("old", Var)) %>% filter(restime_hours<50)
view(resnew)
view(lag_temp)

lag_temp <- lag_temp %>% rename("sample_id" = "Var")
resnew_lag_temp <- merge(resnew, lag_temp, by = c("date", "sample_id"), all = TRUE)
view(resnew_lag_temp)

ggplot(na.omit(resnew_lag_temp), aes(restime_hours,max, color = sample_id))+ 
  geom_point()+
  geom_smooth(method="lm") +
  scale_x_continuous("Residence time (hours)")+
  scale_y_continuous("Lag time (hours)")+
  facet_wrap(~sample_id, ncol = 2) 

ggsave("lag_temp_max_residence_lin.png", width = 8, height = 4)

#calculate lag time based on the max temp differences between dock and bare and dock and eelgrass in hours
lag_do <- daily %>% mutate(bare_max = as.numeric(time_max_do_mgl_Bare - time_max_do_mg_l)/3600,
                             eel_max = as.numeric(time_max_do_mgl_Eelgrass - time_max_do_mg_l)/3600,
                             bare_min = as.numeric(time_min_do_mgl_Bare - time_min_do_mg_l)/3600,
                             eel_min = as.numeric(time_min_do_mgl_Eelgrass - time_min_do_mg_l)/3600) %>% select(bare_max, eel_max, eel_min, bare_min, date) 
lag_do <- lag_do %>% mutate(eel_avg = rowMeans(lag_do[, c('eel_max','eel_min')]),
                                bare_avg = rowMeans(lag_do[, c('bare_max', 'bare_min')]))
lag_do <- lag_do %>% select(eel_max, bare_max, date) %>% pivot_longer(c(-date),names_to = c("Var", ".value"), names_sep = "_")

#plot residence time and do for each tank 
ggplot() + 
  geom_line(data = na.omit(residence), aes(date, restime_hours, color = "Residence"), linewidth= 1)+
  geom_point(data = na.omit(residence), aes(date, restime_hours, color = "Residence"))+
  geom_line(data = na.omit(lag_do), aes(date, max, color = "Lag"), linewidth= 1)+
  geom_point(data = na.omit(lag_do), aes(date,max, color = "Lag"))+
  theme_set(theme_bw())+
  scale_color_manual(values=c("gold", "royalblue"))+
  facet_wrap(~sample_id, 
             ncol= 2)

ggsave("lag_do_max_residence_series.png")


lag_do <- lag_do %>% rename("sample_id" = "Var")
resnew_lag_do <- merge(resnew, lag_do, by = c("date", "sample_id"), all = TRUE)
view(resnew_lag_do)


ggplot(na.omit(resnew_lag_do), aes(restime_hours,max, color = sample_id))+ 
  geom_point()+
  geom_smooth(method="lm") +
  scale_x_continuous("Residence time (hours)")+
  scale_y_continuous("Lag time (hours)")+
  facet_wrap(~sample_id, ncol = 2) 

ggsave("lag_do_max_residence_lin.png", height = 4, width = 8)

