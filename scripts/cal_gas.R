#check the calgas readings over time
#Finn Roach

require(pacman)
p_load(tidyverse, 
       parsedate,
       plotly,
       janitor,
       readxl,
       hms,
       lubridate)

#load in the biweekly sampling sheet of the excel 
calgas <- read_excel("data/Bi-weekly_Tank_Sampling_2023.xlsx",2) %>% 
  select(contains("Sample ID"), contains("CO2 mean"), contains("CO2 stdev"), contains("Date"), contains("Time Collected (PSD)")) %>%
  clean_names() 

#for some reason the times in this sheet get loaded in as decimals (ie 0.5 is noon)
#manually convert to seconds and drop anything that doesnt have a co2 measurement
calgas <- calgas %>% mutate(time_collected_psd = as.numeric(time_collected_psd)*3600*24) %>% drop_na(co2_mean)

#replace NAs in time with noon and convert seconds to hours:minutes:seconds
calgas <- calgas %>% mutate(time_collected_psd = as_hms(time_collected_psd)) %>% replace_na(list(time_collected_psd = 43200))

#select all the calgas rows and create a datetime column
calgas <- calgas %>% filter(grepl('cal', sample_id, ignore.case=TRUE)) %>%
  mutate(datetime = as.POSIXct(paste(parse_date(date), time_collected_psd)))

#load in timeseries sheet
calgas_timeseries <- read_excel("data/Bi-weekly_Tank_Sampling_2023.xlsx",4) %>% 
  select(contains("Sample ID"), contains("CO2 mean"), contains("CO2 stdev"), contains("Date"), contains("Time Collected (PSD)")) %>%
  clean_names()

#for some reason the times get saved with a random date attached, reformat to lose the date
calgas_timeseries$time_collected_psd <- format(ymd_hms(calgas_timeseries$time_collected_psd), "%H:%M:%S")

#fill NAs in time with noon and drop anything with out a CO2 value
calgas_timeseries <- calgas_timeseries %>% replace_na(list(time_collected_psd ="12:00:00")) %>% drop_na(co2_mean)

#select all the calgas rows and create a datetime column
calgas_timeseries <- calgas_timeseries %>% filter(grepl('cal', sample_id, ignore.case=TRUE)) %>%
  mutate(datetime = as.POSIXct(paste(parse_date(date), time_collected_psd)))

#bind the calgas and timeseries dataframes
all_cal <- rbind(calgas, calgas_timeseries) 

#can change slice to drop first row which contains an outlier 
#make sure everything is numeric, select only the necessary columns
all_cal <- all_cal %>% select(co2_mean, co2_stdev, datetime) %>% 
  mutate_at(c('co2_mean', 'co2_stdev'), as.numeric)  %>% slice(1:n())

#view the dataframe
view(all_cal)

#find the mean and stdev of the CO2 values 
summarise(all_cal, cal_mean = mean(co2_mean), cal_stdev = sd(co2_mean))

#plot a linear regression of the cal gas readings over time 
ggplot(data = all_cal, mapping = aes(x = datetime, y = co2_mean)) + geom_point() + 
  geom_smooth(method = "lm", color = "darkgreen") + 
  labs(title = "Calibration gas readings", x = "Date", y = bquote(CO[2]~"(ppm)")) +
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5))

#summarise the statistics from this linear regression
summary(lm(co2_mean~datetime, data = all_cal))

#save the plot
ggsave("cal_gas.png")
