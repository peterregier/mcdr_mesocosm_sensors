#Finn Roach 
#Code to correct csense data based on an offset, shift whole turner cal line up 

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

#calibration times, 410 ppm cal gas 
bare_start <- "2023-08-07 15:25"
bare_end <- "2023-08-07 15:51"
eel_start <- "2023-08-07 15:40"
eel_end <- "2023-08-07 16:05"

#bare = SEVoltage_1, serial number: 24100179
#intercept from turner calibration: (co2 cal gas ppm, sensor output V) = (0, 0.018)
#eelgrass = SEVoltage_2, serial number: 24100180
#intercept from turner calibration: (co2 cal gas ppm, sensor output V) = (0, 0.015)
#slopes and y intercepts from turner sheets, divide slopes by 1000 to convert to mv
m_bare = 403.49/1000
m_eel = 403.08/1000
b_bare = -15.22
b_eel = -9.4862

#select the calibration times 
cal_bare <- df_raw[df_raw$datetime>bare_start&df_raw$datetime<bare_end,] %>% select(datetime, SEVoltage_1)
cal_eel <- df_raw[df_raw$datetime>eel_start&df_raw$datetime<eel_end,] %>% select(datetime, SEVoltage_2)
#visualize the calibrations
ggplot(cal_bare, aes(datetime, SEVoltage_1)) + geom_line()
ggplot(cal_eel, aes(datetime, SEVoltage_2)) + geom_line()
view(cal_bare)
#select the most stable part of these curves
cal_bare <- cal_bare %>% slice(14:16)
cal_eel <- cal_eel %>% slice(15:20)

#from the above calibration datasets we get voltages for 410 ppm cal gas 
x_bare <- mean(cal_bare$SEVoltage_1)
x_eel <- mean(cal_eel$SEVoltage_2)

#we can use this information to find the offset for our calibration line 
off_bare = x_bare - (410-b_bare)/m_bare
off_eel = x_eel - (410-b_eel)/m_eel

#now we can construct our datasets using the equation y = m(x-offset)+ b
df_raw <- df_raw %>% mutate(co2_ppm_bare = m_bare*(SEVoltage_1-off_bare)+b_bare,
                            co2_ppm_eelgrass = m_eel*(SEVoltage_2-off_eel)+b_eel) %>% 
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

#convert datetime to a character so timezones dont get messed up 
df_raw <- df_raw %>% mutate(datetime_pdt = as.character(datetime_pdt))

#save this dataframe as a new corrected csv file 
write_csv(df_raw, "data/csense_timeseries_corrected.csv")
