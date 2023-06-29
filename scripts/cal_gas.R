#check the calgas readings over time 

require(pacman)
p_load(tidyverse, 
       parsedate,
       plotly,
       janitor)

calgas <- read_delim("data/co2_grab_raw.csv") %>% 
  select(contains("Sample ID"), contains("CO2 mean"), contains("CO2 stdev"), contains("Date"), contains("Time Collected (PSD)")) %>%
  clean_names() 

calgas <- calgas %>% replace_na(list(time_collected_psd ="12:00:00")) %>% drop_na(co2_mean)

calgas <- calgas %>% filter(grepl('cal', sample_id, ignore.case=TRUE)) %>%
  mutate(datetime = as.POSIXct(paste(parse_date(date), time_collected_psd)))

calgas_timeseries <- read_delim("data/co2_grab_timeseries_raw.csv") %>% 
  select(contains("Sample ID"), contains("CO2 mean"), contains("CO2 stdev"), contains("Date"), contains("Time Collected (PSD)")) %>%
  clean_names()

calgas_timeseries <- calgas_timeseries %>% replace_na(list(time_collected_psd ="12:00:00")) %>% drop_na(co2_mean)

calgas_timeseries <- calgas_timeseries %>% filter(grepl('cal', sample_id, ignore.case=TRUE)) %>%
  mutate(datetime = as.POSIXct(paste(parse_date(date), time_collected_psd)))


all_cal <- rbind(calgas, calgas_timeseries) 

#drop the first row which contains an outlier
#make sure everything is numberic, select only the necessary columns
all_cal <- all_cal %>% select(co2_mean, co2_stdev, datetime) %>% 
  mutate_at(c('co2_mean', 'co2_stdev'), as.numeric)  %>% slice(1:n())


view(all_cal)

summarise(all_cal, cal_mean = mean(co2_mean), cal_stdev = sd(co2_mean))

p <- ggplot(data = all_cal, mapping = aes(x = datetime, y = co2_mean)) + geom_line()

ggplotly(p)

