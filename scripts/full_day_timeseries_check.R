#Code to plot a full day timeseries and do a linear regression against csense data from the nearest timestamps 

require(pacman)
p_load(tidyverse, 
       parsedate,
       janitor,
       readxl,
       hms,
       lubridate,
       data.table)

#load in timeseries sheet
timeseries <- read_excel("data/Bi-weekly_Tank_Sampling_2023.xlsx",4) %>% 
  select(contains("Sample ID"), contains("CO2 mean"), contains("CO2 stdev"), contains("Date"), contains("Time Collected (PSD)")) %>%
  clean_names() %>% slice(39:73)
view(timeseries)
#for some reason the times get saved with a random date attached, reformat to lose the date
timeseries$time_collected_psd <- format(ymd_hms(timeseries$time_collected_psd), "%H:%M:%S")
#select all the calgas rows and create a datetime column
timeseries <- timeseries %>% filter(grepl(paste(c(3,5), collapse='|'), sample_id)) %>%
  mutate(datetime = as.POSIXct(paste(parse_date(date), time_collected_psd))) 

#select the relevant columns, make sure the timezone is in PDT
#also, multiply the CO2 means by 1.052 to account for the volume of gas used to create the headspace
grabs <- timeseries %>% select(sample_id, co2_mean, datetime) %>%
  mutate(datetime = force_tz(datetime, tz = "America/Los_Angeles"),
         co2_mean = 1.052*co2_mean)

grabs <- grabs %>% pivot_wider(names_from = sample_id, values_from = co2_mean)
grabs <- grabs %>% rename("co2_5"="5", "co2_3"="3")

#plot the full day timeseries 
ggplot()+ 
  geom_point(data = grabs, aes(datetime, co2_5, color = "Bare"))+
  geom_smooth(data = grabs, aes(datetime, co2_5, color="Bare"), method="loess")+
  geom_point(data = grabs, aes(datetime, co2_3, color = "Eelgrass"))+
  geom_smooth(data = grabs, aes(datetime, co2_3, color="Eelgrass"), method="loess")+
  labs(title="CO2 timeseries", x="Time", y="CO2 (ppm)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("blue","darkgreen"))
ggsave("full_day_timeseries.png")

#reads in the csense data 
csense <-  read_delim("data/csense_timeseries_raw.csv") %>% 
  select(datetime_pdt, co2_ppm_bare, co2_ppm_eelgrass)

#makes sure timezone is in PDT 
csense$datetime_pdt <- force_tz(csense$datetime_pdt, tz = "America/Los_Angeles")

#generate time matched dataframe to do linear regression of grab samples against csense 

#converts data frames to data tables
grabs <- data.table(grabs)
csense <- data.table(csense)   

#sets keys to align timecodes 
setkey(grabs, "datetime")
setkey(csense, "datetime_pdt")

#aligns the sensor and grab sample data by nearest timecode 
aligned <- csense[grabs, roll = TRUE]

aligned_5 <- aligned %>% drop_na(co2_5)  %>% select(datetime_pdt, co2_ppm_bare, co2_5)
aligned_3 <- aligned %>% drop_na(co2_3)  %>% select(datetime_pdt, co2_ppm_eelgrass, co2_3)

aligned_5 <- aligned_5 %>% rename("co2"="co2_5", "co2_ppm"="co2_ppm_bare")
aligned_3 <- aligned_3 %>% rename("co2"="co2_3", "co2_ppm"="co2_ppm_eelgrass")

stacked <- rbind(aligned_5, aligned_3)

#view(stacked)

#stacked <- stacked[!12,]

#head(aligned_5)

#plot everything 

ggplot(aligned_5, aes(co2_ppm, co2))+ 
  geom_point()+
  geom_smooth(method="lm",color="blue")+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color="blue")+
  labs(title="CO2 Zero Density Eelgrass", x="CSense CO2 Conc (ppm)", y="Grab Sample CO2 Conc (ppm)") +
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12)) + ylim(500,800)

ggsave("co2_bare_full_day.png",width = 4, height = 4)
summary(lm(co2_ppm~co2, data=aligned_5))

ggplot(aligned_3, aes(co2_ppm, co2))+ 
  geom_point()+
  geom_smooth(method="lm",color="darkgreen")+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color="darkgreen")+
  labs(title="CO2 High Density Eelgrass", x="CSense CO2 Conc (ppm)", y="Grab Sample CO2 Conc (ppm)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))

ggsave("co2_eelgrass_full_day.png", width = 4, height = 4)
summary(lm(co2_ppm~co2, data=aligned_3))

ggplot(stacked, aes(co2_ppm, co2))+
  geom_point()+ 
  geom_smooth(method="lm", color="turquoise")+ 
  labs(title="CO2", x="CSense CO2 Conc (ppm)", y="Grab Sample CO2 Conc (ppm)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))

ggsave("co2.png",width = 4, height = 4)
summary (lm(co2_ppm~co2, data = stacked))
