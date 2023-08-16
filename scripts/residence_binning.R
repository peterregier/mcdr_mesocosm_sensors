require(pacman)
p_load(tidyverse,
       janitor,
       lubridate,
       gridExtra,
       plotly,
       readxl,
       hms)

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
#CORRECTIONS
#offset for 5/29 to 7/25 based on calibration pre-clean checks  
eel_sal_offset <-2.08
bare_sal_offset <- 2.07
exodata <- exodata[order(exodata$datetime_pdt),]
exodata <- exodata %>% mutate(sal_psu_Bare = c(sal_psu_Bare[1:16643] + bare_sal_offset, sal_psu_Bare[16644:nrow(exodata)]),
                              sal_psu_Eelgrass = c(sal_psu_Eelgrass[1:16643] + eel_sal_offset, sal_psu_Eelgrass[16644:nrow(exodata)]))
exodata <- exodata %>% select(-datetime_pdt, -p_havg_Eelgrass, -p_havg_Bare)


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
flow$old_flow <- c(flow$old_flow[2:nrow(flow)], NA)
flow <- flow %>% mutate(avg_flow = rowMeans(flow[,c('old_flow','new_flow')]))
flow <- flow %>% pivot_longer(c(-date, -sample_id, -do_surface, -t_surface, -avg_flow),
                              names_to = c("Var", ".value"),
                              names_sep = "_")
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
residence <- residence %>% pivot_wider(names_from = sample_id, values_from = restime_hours) 
residence <- merge(na.omit(residence[c("date","eel")]),na.omit(residence[c("date","bare")]), by = "date")
#can also define bins with: eel_bin = cut(eel, breaks = c(0,4,6,8,10), labels = FALSE
residence <- residence %>% mutate(eel_bin = cut(eel, breaks = c(0,4.75,10), labels = FALSE),
                                  bare_bin = cut(bare, breaks = c(0,4.75,10), labels = FALSE))

exores <- merge(residence, exodata, by = "date", all = TRUE)


#BREAK UP AVERAGES BY BIN?!


#create dataframes for the daily average plots and graph them 
exotemp_bare_2 <- exores %>% filter(grepl("2", bare_bin)) %>%
  select(time,temp_c_Bare) %>% 
  group_by(time) %>% 
  summarize(mean = mean(temp_c_Bare, na.rm = T), 
            sd = sd(temp_c_Bare, na.rm = T))

#create dataframes for the daily average plots and graph them 
exotemp_bare_1 <- exores %>% filter(grepl("1", bare_bin)) %>%
  select(time,temp_c_Bare) %>% 
  group_by(time) %>% 
  summarize(mean = mean(temp_c_Bare, na.rm = T), 
            sd = sd(temp_c_Bare, na.rm = T))


CTDtemp <- CTD %>% 
  select(time,temp_deg_c) %>% 
  group_by(time) %>% 
  summarize(mean = mean(temp_deg_c, na.rm = T), 
            sd = sd(temp_deg_c, na.rm = T)) 

ggplot(data = exotemp_bare_1, aes(time,mean, color = "Short residence time")) + 
  geom_smooth(data = exotemp_bare_2, aes(time, mean, color = "Long residence time")) + 
  geom_smooth(data = CTDtemp, aes(time, mean, color = "Dock")) + 
  geom_errorbar(data = exotemp_bare_1, aes(ymin = mean - sd, ymax = mean + sd, color = "Short residence time"), alpha = 0.2) +
  geom_errorbar(data = exotemp_bare_2, aes(ymin = mean - sd, ymax = mean + sd, color = "Long residence time"), alpha = 0.2) +
  geom_errorbar(data = CTDtemp, aes(ymin = mean - sd, ymax = mean + sd, color = "Dock"), alpha = 0.2) +
  geom_smooth() + labs(title="Mean daily temperature bare tank", x="Time", y="Mean Temperature (C)") + theme_set(theme_bw()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("maroon", "navyblue","skyblue"))
ggsave("temp_bare_binned.png", height = 4, width = 6)

#create dataframes for the daily average plots and graph them 
exotemp_eel_2 <- exores %>% filter(grepl("2", eel_bin)) %>%
  select(time,temp_c_Eelgrass) %>% 
  group_by(time) %>% 
  summarize(mean = mean(temp_c_Eelgrass, na.rm = T), 
            sd = sd(temp_c_Eelgrass, na.rm = T))

#create dataframes for the daily average plots and graph them 
exotemp_eel_1 <- exores %>% filter(grepl("1", eel_bin)) %>%
  select(time,temp_c_Eelgrass) %>% 
  group_by(time) %>% 
  summarize(mean = mean(temp_c_Eelgrass, na.rm = T), 
            sd = sd(temp_c_Eelgrass, na.rm = T))

ggplot(data = exotemp_eel_1, aes(time,mean, color = "Short residence time")) + 
  geom_smooth(data = exotemp_eel_2, aes(time, mean, color = "Long residence time")) +
  geom_smooth(data = CTDtemp, aes(time, mean, color = "Dock")) + 
  geom_errorbar(data = exotemp_eel_1, aes(ymin = mean - sd, ymax = mean + sd, color = "Short residence time"), alpha = 0.2) +
  geom_errorbar(data = exotemp_eel_2, aes(ymin = mean - sd, ymax = mean + sd, color = "Long residence time"), alpha = 0.2) +
  geom_errorbar(data = CTDtemp, aes(ymin = mean - sd, ymax = mean + sd, color = "Dock"), alpha = 0.2) +
  geom_smooth() + labs(title="Mean daily temperature eelgrass tank", x="Time", y="Mean Temperature (C)") + theme_set(theme_bw()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("maroon", "darkgreen","lightgreen"))
ggsave("temp_eel_binned.png", height = 4, width = 6)


#create dataframes for the daily average plots and graph them 
exodo_bare_2 <- exores %>% filter(grepl("2", bare_bin)) %>%
  select(time,do_mgl_Bare) %>% 
  group_by(time) %>% 
  summarize(mean = mean(do_mgl_Bare, na.rm = T), 
            sd = sd(do_mgl_Bare, na.rm = T))

#create dataframes for the daily average plots and graph them 
exodo_bare_1 <- exores %>% filter(grepl("1", bare_bin)) %>%
  select(time,do_mgl_Bare) %>% 
  group_by(time) %>% 
  summarize(mean = mean(do_mgl_Bare, na.rm = T), 
            sd = sd(do_mgl_Bare, na.rm = T))

CTDdo <- CTD %>% 
  select(time,do_mg_l) %>% 
  group_by(time) %>% 
  summarize(mean = mean(do_mg_l, na.rm = T), 
            sd = sd(do_mg_l, na.rm = T)) 

ggplot(data = exodo_bare_1, aes(time,mean, color = "Short residence time")) + 
  geom_smooth(data = exodo_bare_2, aes(time, mean, color = "Long residence time")) +
  geom_smooth(data = CTDdo, aes(time, mean, color = "Dock")) + 
  geom_errorbar(data = exodo_bare_1, aes(ymin = mean - sd, ymax = mean + sd, color = "Short residence time"), alpha = 0.2) +
  geom_errorbar(data = exodo_bare_2, aes(ymin = mean - sd, ymax = mean + sd, color = "Long residence time"), alpha = 0.2) +
  geom_errorbar(data = CTDdo, aes(ymin = mean - sd, ymax = mean + sd, color = "Dock"), alpha = 0.2) +
  geom_smooth() + labs(title="Mean daily DO bare tank", x="Time", y="Dissolved oxygen (mgl)") + theme_set(theme_bw()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("maroon", "navyblue","skyblue"))
ggsave("do_bare_binned.png", height = 4, width = 6)

#create dataframes for the daily average plots and graph them 
exodo_eel_2 <- exores %>% filter(grepl("2", eel_bin)) %>%
  select(time,do_mgl_Eelgrass) %>% 
  group_by(time) %>% 
  summarize(mean = mean(do_mgl_Eelgrass, na.rm = T), 
            sd = sd(do_mgl_Eelgrass, na.rm = T))

#create dataframes for the daily average plots and graph them 
exodo_eel_1 <- exores %>% filter(grepl("1", eel_bin)) %>%
  select(time,do_mgl_Eelgrass) %>% 
  group_by(time) %>% 
  summarize(mean = mean(do_mgl_Eelgrass, na.rm = T), 
            sd = sd(do_mgl_Eelgrass, na.rm = T))

ggplot(data = exodo_eel_1, aes(time,mean, color = "Short residence time")) + 
  geom_smooth(data = exodo_eel_2, aes(time, mean, color = "Long residence time")) +
  geom_smooth(data = CTDdo, aes(time, mean, color = "Dock")) + 
  geom_errorbar(data = exodo_eel_1, aes(ymin = mean - sd, ymax = mean + sd, color = "Short residence time"), alpha = 0.2) +
  geom_errorbar(data = exodo_eel_2, aes(ymin = mean - sd, ymax = mean + sd, color = "Long residence time"), alpha = 0.2) +
  geom_errorbar(data = CTDdo, aes(ymin = mean - sd, ymax = mean + sd, color = "Dock"), alpha = 0.2) +
  geom_smooth() + labs(title="Mean daily DO eelgrass tank", x="Time", y="Dissolved oxygen (mgl)") + theme_set(theme_bw()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("maroon", "darkgreen","lightgreen"))
ggsave("do_eel_binned.png", height = 4, width = 6)



