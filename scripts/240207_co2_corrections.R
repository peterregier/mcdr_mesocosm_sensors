## This script is based on Finn's csense_correction_timeseries.R script. Notes from Finn:
# I tried doing a calibration by forcing air through the pumped head, but I didn't
# have enough time to work on that method before I left so I didn't get a good
# calibration down. So instead of using any of this calibration check data, I used
# the grab sample timeseries data (I think from one full day timeseries), and I
# paired each CO2 ppm measurement given by picaro with the csense voltage data point
# that was taken nearest to when the grab sample was collected. This is assuming
# that picaro was accurate enough that we could treat those points as calibration
# points (which is a flawed assumption for sure, but I think it was our best option).
# I then had a script that constructs a calibration line (voltage vs ppm essentially)
# with this data, and also the zero data point from the factory calibration
# (I kept this because we didn't have anything close to a zero from the picaro data).
# It then applies this calibration line to the voltage data from csense.  The script
# that does this is called csense_correction_timeseries.R and is under scripts in the github.
## tl;dr - we're correcting C-Sense mv to grab samples by matching a time-series
## 1. Read in CO2 time-series
## 2. Read in grab samples
## 3. Match and plot (that's basically Figure 4)
## 4. Export for Fig 4
## 5. Export corrected CO2 for Fig 8

# 1. Setup ---------------------------------------------------------------------
source("scripts/0_constants.R")

csense <- read_csv("data/240207_timeseries_raw.csv")

## Set threshold for flagging CO2 based on standard deviation
co2_sd_threshold = 50 #if the sd > threshold, cull

#set new time interval in this script, different than in the constants script 
#this is just to allow us to get as many data points as possible for our calibration 
time_interval_cal = "1 min"

## Helper functions
mean_ <- function(var){mean({{var}}, na.rm = T)}
sd_ <- function(var){sd({{var}}, na.rm = T)}

## Read in grab samples
## multiply the CO2 means by 1.052 to account for the volume of gas used to create the headspace
grab_samples <- read_excel("data/Bi-weekly_Tank_Sampling_2023.xlsx",4) %>% 
  select(contains("Sample ID"), contains("CO2 mean"), contains("CO2 stdev"), contains("Date"), contains("Time Collected (PSD)")) %>%
  clean_names() %>% 
  mutate(date = as_date(date), 
         time_collected_psd = as_hms(time_collected_psd), 
         datetime = round_date(as_datetime(paste(date, time_collected_psd)), "5 min")) %>% 
  mutate(co2_mean = 1.052*co2_mean, 
         tank = case_when(sample_id == 3 ~ "Bare", 
                          sample_id == 5 ~ "Eelgrass", 
                          TRUE ~ NA))

#grab_sample_start <- "2023-07-13 09:00"
#grab_sample_end <- "2023-07-13 16:10"


df1 <- inner_join(csense, grab_samples, 
                 by = c("datetime", "tank")) %>% 
  #filter(datetime > "2023-08-15") %>% # only keep 8/16 samples
  select(datetime, contains("co2_m"), tank)

ggplot(df1, aes(co2_mean, co2_mv, color = as.factor(as_date(datetime)))) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  #stat_poly_eq() +
  facet_wrap(~tank) + 
  labs(x = "Grab-sample CO2 (ppm)", 
       y = "Sensor CO2 (mV)")
ggsave("figures/240207_grab_v_sensor_co2_all.png", width = 7, height = 3)

df1_07_08 <- df1 %>% 
  filter(datetime > "2023-07-10") 
write_csv(df1_07_08, "data/240207_co2_713_816_for_fig4.csv")

ggplot(df1_07_08, aes(co2_mean, co2_mv)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  #stat_poly_eq() +
  facet_wrap(tank~as_date(datetime)) + 
  labs(x = "Grab-sample CO2 (ppm)", 
       y = "Sensor CO2 (mV)")
ggsave("figures/240207_grab_v_sensor_co2_713_816.png", width = 7, height = 6)


#bare = SEVoltage_1, serial number: 24100179
#intercept from turner calibration: (co2 cal gas ppm, sensor output V) = (0, 0.018)
#eelgrass = SEVoltage_2, serial number: 24100180
#intercept from turner calibration: (co2 cal gas ppm, sensor output V) = (0, 0.015)
#multiply the turner values by 1000 to convert into volts 
a_bare = 0.018*1000
a_eel = 0.015*1000

## This is the start for the "good" data we'll use
fig8_start <- "2023-07-28"

df2 <- df1_07_08 %>% 
  filter(datetime > fig8_start) #%>% 
  #add_row(datetime = as_datetime("2023-08-16"), co2_mv = a_bare, co2_mean = 0, tank = "Bare") %>% 
  #add_row(datetime = as_datetime("2023-08-16"), co2_mv = a_eel, co2_mean = 0, tank = "Eelgrass")

## Adding in rows above to force y-intercepts doesn't do a good job, if the intent
## is simply to ground-truth sensors to Picarro, let's 
ggplot(df2, aes(co2_mean, co2_mv, color = tank)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)

## Let's pivot and just find m and b, then correct CO2. For y=mx+b, we want to
## predict ppm (Picarro), so that's y (contrary to relationships above)
bare_lm <- summary(lm(co2_mean ~ co2_mv, data = df2 %>% filter(tank == "Bare")))
eelgrass_lm <- summary(lm(co2_mean ~ co2_mv, data = df2 %>% filter(tank == "Eelgrass")))

## Cheatsheet to remember which is which
bare_lm[[4]][2,1] #slope
bare_lm[[4]][1,1] #int
round(bare_lm[[9]], 2) #r2

## Double-check that corrections make sense prior to applying to dataset
df2 %>% 
  mutate(slope = case_when(tank == "Bare" ~ bare_lm[[4]][2,1], 
                           tank == "Eelgrass" ~ eelgrass_lm[[4]][2,1]), 
         int = case_when(tank == "Bare" ~ bare_lm[[4]][1,1], 
                         tank == "Eelgrass" ~ eelgrass_lm[[4]][1,1])) %>% 
  mutate(co2_ppm_calc = (co2_mv * slope) + int)

csense_cor <- csense %>% 
  mutate(slope = case_when(tank == "Bare" ~ bare_lm[[4]][2,1], 
                           tank == "Eelgrass" ~ eelgrass_lm[[4]][2,1]), 
         int = case_when(tank == "Bare" ~ bare_lm[[4]][1,1], 
                         tank == "Eelgrass" ~ eelgrass_lm[[4]][1,1])) %>% 
  mutate(co2_ppm_calc = (co2_mv * slope) + int)
write_csv(csense_cor, "data/240207_timeseries_corrected.csv")




