require(pacman)
p_load(tidyverse, 
       parsedate,
       janitor,
       readxl,
       hms,
       lubridate,
       data.table)

#load in biweekly datasheet 
biweekly <- read_excel("data/Bi-weekly_Tank_Sampling_2023.xlsx",2) %>% 
  select(contains("Sample ID"), contains("CO2 mean"), contains("CO2 stdev"), contains("Date"), contains("Time Collected (PSD)")) %>%
  clean_names() %>% slice(4:n())
#select only the rows corresponding to tanks 3 and 5
biweekly <- biweekly %>% filter(grepl(paste(c(3,5), collapse='|'), sample_id))  %>% drop_na(time_collected_psd)
#for some reason the times in this sheet get loaded in as decimals (ie 0.5 is noon)
#manually convert to seconds and drop anything that doesnt have a co2 measurement
biweekly <- biweekly %>% mutate(time_collected_psd = as.hms(as.numeric(time_collected_psd)*3600*24))
#select all the calgas rows and create a datetime column
biweekly <- biweekly %>% mutate(datetime = as.POSIXct(paste(parse_date(date), time_collected_psd)))

#load in timeseries sheet
timeseries <- read_excel("data/Bi-weekly_Tank_Sampling_2023.xlsx",4) %>% 
  select(contains("Sample ID"), contains("CO2 mean"), contains("CO2 stdev"), contains("Date"), contains("Time Collected (PSD)")) %>%
  clean_names()
#for some reason the times get saved with a random date attached, reformat to lose the date
timeseries$time_collected_psd <- format(ymd_hms(timeseries$time_collected_psd), "%H:%M:%S")
#select all the calgas rows and create a datetime column
timeseries <- timeseries %>% filter(grepl(paste(c(3,5), collapse='|'), sample_id)) %>%
  mutate(datetime = as.POSIXct(paste(parse_date(date), time_collected_psd))) 


#merges and pivots the grab sample data 
grabs <- rbind(timeseries, biweekly)
#select the relevant columns, make sure the timezone is in PDT
#also, multiply the CO2 means by 1.052 to account for the volume of gas used to create the headspace
grabs <- grabs %>% select(sample_id, co2_mean, datetime) %>%
  mutate(datetime = force_tz(datetime, tz = "America/Los_Angeles"),
         co2_mean = 1.052*co2_mean)

grabs <- grabs %>% pivot_wider(names_from = sample_id, values_from = co2_mean)
grabs <- grabs %>% rename("co2_5"="5", "co2_3"="3")


#reads in the csense data 
csense <-  read_delim("data/csense_timeseries_raw.csv") %>% 
  select(datetime_pdt, co2_ppm_bare, co2_ppm_eelgrass)

#makes sure timezone is in PDT 
csense$datetime_pdt <- force_tz(csense$datetime_pdt, tz = "America/Los_Angeles")

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

ggplot(aligned_5, aes(co2_ppm, co2))+ 
  geom_point()+
  geom_smooth(method="lm",color="blue")+
  labs(title="CO2 Zero Density Eelgrass", x="CSense CO2 Conc (ppm)", y="Grab Sample CO2 Conc (ppm)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))

ggsave("co2_bare.png",width = 4, height = 4)
summary(lm(co2_ppm~co2, data=aligned_5))

ggplot(aligned_3, aes(co2_ppm, co2))+ 
  geom_point()+
  geom_smooth(method="lm",color="green")+
  labs(title="CO2 High Density Eelgrass", x="CSense CO2 Conc (ppm)", y="Grab Sample CO2 Conc (ppm)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))

ggsave("co2_eelgrass.png", width = 4, height = 4)
summary(lm(co2_ppm~co2, data=aligned_3))

ggplot(stacked, aes(co2_ppm, co2))+
  geom_point()+ 
  geom_smooth(method="lm", color="turquoise")+ 
  labs(title="CO2", x="CSense CO2 Conc (ppm)", y="Grab Sample CO2 Conc (ppm)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))

ggsave("co2.png",width = 4, height = 4)
summary (lm(co2_ppm~co2, data = stacked))
