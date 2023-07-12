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
biweekly$time_collected_psd <- format(ymd_hms(timeseries$time_collected_psd), "%H:%M:%S")

view(biweekly)
#load in timeseries sheet
timeseries <- read_excel("data/Bi-weekly_Tank_Sampling_2023.xlsx",4) %>% 
  select(contains("Sample ID"), contains("CO2 mean"), contains("CO2 stdev"), contains("Date"), contains("Time Collected (PSD)")) %>%
  clean_names()
#for some reason the times get saved with a random date attached, reformat to lose the date
timeseries$time_collected_psd <- format(ymd_hms(timeseries$time_collected_psd), "%H:%M:%S")
#select all the calgas rows and create a datetime column
timeseries <- timeseries %>% filter(grepl(paste(c(3,5), collapse='|'), sample_id)) %>%
  mutate(datetime = as.POSIXct(paste(parse_date(date), time_collected_psd))) 

view(timeseries)
#merges and pivots the grab sample data 
grabs <- rbind(timeseries, biweekly)
#select the relevant columns, make sure the timezone is in PDT
#also, multiply the CO2 means by 1.052 to account for the volume of gas used to create the headspace
grabs <- grabs %>% select(sample_id, co2_mean, datetime, time_collected_psd) %>%
  mutate(datetime = force_tz(datetime, tz = "America/Los_Angeles"),
         co2_mean = 1.052*co2_mean)
grabs$time_collected_psd <- format(ymd_hms(grabs$datetime), "%H:%M:%S")
grabs <- grabs %>% mutate(time_collected_psd = as.POSIXct(paste(parse_date("1111-11-11"), time_collected_psd)))

grabs <- grabs %>% pivot_wider(names_from = sample_id, values_from = co2_mean)
grabs <- grabs %>% rename("co2_5"="5", "co2_3"="3")
view(grabs)

bare <- grabs %>% drop_na(co2_5)  %>% select(time_collected_psd, co2_5)
eel <- grabs %>% drop_na(co2_3)  %>% select(time_collected_psd, co2_3)

bare <- bare %>% rename("co2"="co2_5")

eel <- eel %>% rename("co2"="co2_3")
view(bare)
view(eel)
stacked <-  rbind(bare, eel)
view(stacked)

ggplot()+ 
  geom_point(data = bare, aes(time_collected_psd, co2, color = "Bare"))+
  geom_smooth(data = bare, aes(time_collected_psd, co2, color="Bare"), method="loess")+
  geom_point(data = eel, aes(time_collected_psd, co2, color = "Eelgrass"))+
  geom_smooth(data = eel, aes(time_collected_psd, co2, color="Eelgrass"), method="loess")+
  labs(title="CO2 timeseries", x="Time", y="CO2 (ppm)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("blue","darkgreen"))

ggsave("co2_grabs_timeseries.png",width = 5, height = 4)
