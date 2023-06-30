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
head(biweekly)
#for some reason the times in this sheet get loaded in as decimals (ie 0.5 is noon)
#manually convert to seconds and drop anything that doesnt have a co2 measurement
biweekly <- biweekly %>% mutate(time_collected_psd = as.hms(as.numeric(time_collected_psd)*3600*24))
head(biweekly)
#select all the calgas rows and create a datetime column
biweekly <- biweekly %>% mutate(datetime = as.POSIXct(paste(parse_date(date), time_collected_psd)))

#load in timeseries sheet
timeseries <- read_excel("data/Bi-weekly_Tank_Sampling_2023.xlsx",4) %>% 
  select(contains("Sample ID"), contains("CO2 mean"), contains("CO2 stdev"), contains("Date"), contains("Time Collected (PSD)")) %>%
  clean_names()
head(timeseries)
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
view(grabs)

#reads in the csense data 
csense <-  read_delim("data/csense_timeseries_raw.csv") %>% 
  select(datetime_raw, co2_ppm_bare, co2_ppm_eelgrass)
#view(csense)

csense$datetime_raw <-  force_tz(csense$datetime_raw, tz = "America/Los_Angeles")

#view(csense)
grabs <- data.table(grabs)
csense <- data.table(csense)   

setkey(grabs, "datetime")
setkey(csense, "datetime_raw")

aligned <- csense[grabs, roll = TRUE]

view(aligned)
