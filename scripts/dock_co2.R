#Finn Roach 
#Code to plot dock and tank timeseries on same graph 

#load required packages 
require(pacman)
p_load(tidyverse,
       janitor,
       lubridate,
       gridExtra,
       plotly,
       hms,
       readxl)

#reads in the CORRECTED csense data 
csense <-  read_delim("data/csense_timeseries_corrected.csv") %>% 
  select(datetime_pdt, co2_ppm_bare, co2_ppm_eelgrass)
#makes sure timezone is in PDT and remove maintenance data 
csense <- csense %>% mutate(datetime_pdt = as.POSIXct(datetime_pdt)) 
csense <- csense %>% mutate(datetime_pdt = force_tz(datetime_pdt, tz = "America/Los_Angeles"))
csense <-  csense %>% mutate(time = format(as.POSIXct(datetime_pdt), format = "%H:%M:%S")) 
csense <- csense %>% mutate(time = as_hms(time))

#reads in dock CO2 data 
dock <- read_excel("data/dock_pCO2_data.xlsx") %>% clean_names()
dock <- dock %>% mutate(time = as.POSIXct(time, tz = "GMT", tryFormats = c("%m/%d/%Y %H:%M:%OS",
                                                                                 "%m/%d/%z %H:%M",
                                                                                 "%m/%d/%Y %H:%M",
                                                                                 "%m-%d-%z %H:%M")))
dock <- dock %>% mutate(datetime_pdt = with_tz(time, tz = "America/Los_Angeles"))
dock <- dock %>% mutate(time = format(as.POSIXct(datetime_pdt), format = "%H:%M:%S")) 
dock <- dock %>% mutate(time = as_hms(time))

#select time from csense to plot 
start <- "2023-08-14 20:00:00"
end <- "2023-08-20 24:00:00"
csense <-  csense[csense$datetime_pdt>start&csense$datetime_pdt<end,] %>% select(datetime_pdt, co2_ppm_bare, co2_ppm_eelgrass)

#generate co2 timeseries graph
co2 <- ggplot() + 
  geom_line(data = csense, aes(datetime_pdt, co2_ppm_bare, color = "Bare"))+
  geom_line(data = csense, aes(datetime_pdt, co2_ppm_eelgrass, color = "Eelgrass"))+
  geom_line(data = dock, aes(datetime_pdt, co2_ppm, color = "Dock"))+
  labs(title="CO2 timeseries", x="Time", y="CO2 (ppm)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("royalblue","maroon", "forestgreen"))


#create dataframes for the daily average plots and graph them 
co2_bare <- csense %>% 
  select(time, co2_ppm_bare) %>% 
  group_by(time) %>% 
  summarize(mean = mean(co2_ppm_bare, na.rm = T), 
            sd = sd(co2_ppm_bare, na.rm = T))
co2_eel <- csense %>% 
  select(time, co2_ppm_eelgrass) %>% 
  group_by(time) %>% 
  summarize(mean = mean(co2_ppm_eelgrass, na.rm = T), 
            sd = sd(co2_ppm_eelgrass, na.rm = T))
co2_dock <- dock %>% 
  select(time, co2_ppm) %>% 
  group_by(time) %>% 
  summarize(mean = mean(co2_ppm, na.rm = T), 
            sd = sd(co2_ppm, na.rm = T))

co2_avg <- ggplot(data = co2_bare, aes(time,mean, color = "Bare")) + 
  geom_smooth(data = co2_eel, aes(time, mean, color = "Eelgrass")) + 
  geom_smooth(data = co2_dock, aes(time, mean, color = "Dock")) +
  geom_errorbar(data = co2_bare, aes(ymin = mean - sd, ymax = mean + sd, color = "Bare"), alpha = 0.2) +
  geom_errorbar(data = co2_eel, aes(ymin = mean - sd, ymax = mean + sd, color = "Eelgrass"), alpha = 0.2) +
  geom_errorbar(data = co2_dock, aes(ymin = mean - sd, ymax = mean + sd, color = "Dock"), alpha = 0.2) +
  geom_smooth() + labs(title="Mean daily CO2", x="Time", y="Mean CO2 (ppm)") + theme_set(theme_bw()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("royalblue","maroon", "forestgreen"))

#plot the timeseries and daily averages side by side 
co2_plot<- grid.arrange(co2,co2_avg, widths = c(1, 0.4))

ggsave("co2_plot.png",co2_plot, width = 14, height = 4)

