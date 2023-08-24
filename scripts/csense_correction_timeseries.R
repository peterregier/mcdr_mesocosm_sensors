#Finn Roach 
#Code to correct csense data based on an 8 point full day timeseries
#using this script to correct csense data for deliverables
#keep the c-sense zero point 

require(pacman)
p_load(tidyverse,
       janitor,
       lubridate,
       gridExtra,
       plotly,
       hms)

#code below adapted from peter to load in csense data from datalogger files 

## Location of raw csense datasets
raw_filepath = "data/csense/timeseries"

## Load constants
source("scripts/0_constants.R")

#read in the data

## Set threshold for flagging CO2 based on standard deviation
co2_sd_threshold = 50 #if the sd > threshold, cull

#set new time interval in this script, different than in the constants script 
#this is just to allow us to get as many data points as possible for our calibration 
time_interval_cal = "1 min"

## Helper functions
mean_ <- function(var){mean({{var}}, na.rm = T)}
sd_ <- function(var){sd({{var}}, na.rm = T)}

files <- list.files(raw_filepath, pattern = "CR6Series_Table1", full.names = T)

#load in the raw voltages to compute new calibration point 
read_cr6 <- function(file){
  df_raw <- read_delim(file, skip = 1) %>% 
    slice(3:n()) %>% 
    mutate(SEVoltage_1 = as.numeric(SEVoltage_1), 
           SEVoltage_2 = as.numeric(SEVoltage_2)) %>% 
    mutate(datetime_1min = force_tz(parsedate::parse_date(TIMESTAMP), tzone = common_tz)) %>% 
    mutate(datetime = round_date(datetime_1min, unit = time_interval_cal)) %>% 
    select(datetime,contains("SEVoltage_")) 
}

df_raw <- files %>% 
  map(read_cr6) %>% 
  bind_rows()

#make sure the data is in pacific time
df_raw$datetime <- force_tz(df_raw$datetime, tz = "America/Los_Angeles")

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
timeseries <- timeseries %>% select(sample_id, co2_mean, datetime) %>%
  mutate(datetime = force_tz(datetime, tz = "America/Los_Angeles"),
         co2_mean = 1.052*co2_mean)
timeseries <- timeseries %>% pivot_wider(names_from = sample_id, values_from = co2_mean)
timeseries <- timeseries %>% rename("co2_bare"="5", "co2_eel"="3")

start <- "2023-07-13 09:00"
end <- "2023-07-13 16:10"

#bare = SEVoltage_1, serial number: 24100179
#intercept from turner calibration: (co2 cal gas ppm, sensor output V) = (0, 0.018)
#eelgrass = SEVoltage_2, serial number: 24100180
#intercept from turner calibration: (co2 cal gas ppm, sensor output V) = (0, 0.015)
#multiply the turner values by 1000 to convert into volts 
a_bare = 0.018*1000
a_eel = 0.015*1000

#select the calibration times and merge the datasets 
cal_bare <- df_raw[df_raw$datetime>start&df_raw$datetime<end,] %>% select(datetime, SEVoltage_1)
cal_eel <- df_raw[df_raw$datetime>start&df_raw$datetime<end,] %>% select(datetime, SEVoltage_2)
bare <- merge(cal_bare, timeseries[,c("co2_bare", "datetime")], by = "datetime", all = TRUE) %>% drop_na()
eel <- merge(cal_eel, timeseries[,c("co2_eel", "datetime")], by = "datetime", all = TRUE) %>% drop_na()
#add the turner zero point to the dataframes
eel[nrow(eel)+1,] <- c("2023-07-13 00:00",a_eel,0)
bare[nrow(bare)+1,] <- c("2023-07-13 00:00",a_bare,0)
eel <- eel %>% mutate(co2_eel = as.numeric(co2_eel),
              SEVoltage_2 = as.numeric(SEVoltage_2))
bare<- bare %>% mutate(co2_bare = as.numeric(co2_bare),
                      SEVoltage_1 = as.numeric(SEVoltage_1))
#plot the calibration lines 
ggplot(bare, aes(SEVoltage_1, co2_bare))+ geom_point()+
  geom_smooth(method="lm")
ggplot(eel, aes(SEVoltage_2, co2_eel))+ geom_point()+
  geom_smooth(method="lm")

#extract slopes and intercepts of these lines 
bare_model <- lm(co2_bare~SEVoltage_1, bare)
bare_coef <- coef(bare_model)
b_bare <- as.numeric(bare_coef[1])
m_bare <- as.numeric(bare_coef[2])
eel_model <- lm(co2_eel~SEVoltage_2, eel)
eel_coef <- coef(eel_model)
b_eel <- as.numeric(eel_coef[1])
m_eel <- as.numeric(eel_coef[2])

#now we can construct our datasets using the y = mx+b equation 
df_raw <- df_raw %>% mutate(co2_ppm_bare = m_bare*(SEVoltage_1)+b_bare,
                            co2_ppm_eelgrass = m_eel*(SEVoltage_2)+b_eel) %>% 
  select(datetime, co2_ppm_bare, co2_ppm_eelgrass)


#reformat the data-set so it uses the 5 min time interval from the constants script 
df_raw <- df_raw %>%
  mutate(datetime_pdt = round_date(datetime, unit = time_interval)) %>% 
  select(datetime_pdt, contains("co2_ppm_"))  %>% 
  group_by(datetime_pdt) %>% 
  summarize(sd_ppm_bare = sd_(co2_ppm_bare), 
            sd_ppm_eelgrass = sd_(co2_ppm_eelgrass),
            co2_ppm_bare = mean_(co2_ppm_bare), 
            co2_ppm_eelgrass = mean_(co2_ppm_eelgrass))

#plot some data! 
start <- "2023-08-07 17:00"
end <- "2023-08-14 8:00"

example <- df_raw[df_raw$datetime_pdt>start&df_raw$datetime_pdt<end,]

p <- ggplot() + 
  geom_line(data = example, aes(datetime_pdt, co2_ppm_bare, color = "Bare"))+
  geom_line(data = example, aes(datetime_pdt, co2_ppm_eelgrass, color = "Eelgrass"))+
  labs(title="CO2 timeseries", x="Time", y="CO2 (ppm)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("royalblue","forestgreen"))

ggplotly(p)

#save datetime as a character so the timezones dont get messed up 
df_raw <- df_raw %>% mutate(datetime_pdt = as.character(datetime_pdt))

#save this dataframe as a new corrected csv file 
write_csv(df_raw, "data/csense_timeseries_corrected.csv")
