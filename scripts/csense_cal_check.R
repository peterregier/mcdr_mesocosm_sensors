#Check the calibration readings for the csense over time
#some code adapted from Peter's scripts 
#Finn Roach 

require(pacman)
p_load(tidyverse, 
       parsedate,
       plotly,
       cowplot)

theme_set(theme_bw())

#read data 

csense_old <- read_delim("data/csense/timeseries/CR6Series_Table1_0526_0612.dat", skip = 1) %>% 
  slice(3:n()) %>% 
  mutate(datetime = parsedate::parse_date(TIMESTAMP), 
         co2_ppm_bare = as.numeric(SEVolt_1), 
         co2_ppm_eelgrass = as.numeric(SEVolt_2)) %>% 
  select(datetime, contains("co2_ppm"))

view(csense_old)

csense_raw <- read_delim("data/csense/timeseries/CR6Series_Table1_0711_0717.dat", skip = 1) %>% 
  slice(3:n()) %>% 
  mutate(datetime = parsedate::parse_date(TIMESTAMP), 
         co2_ppm_bare = as.numeric(SEVolt_1), 
         co2_ppm_eelgrass = as.numeric(SEVolt_2)) %>% 
  select(datetime, contains("co2_ppm"))

view(csense_raw)

#select the calibration data and then take the means once the calibration has stabilized, if possible 
#unfortunately the best way to do this is probably manually at the moment 

#select only 7/11 cal data 
csense_cal_711  <- csense_raw %>% slice(41465:41475)
#take average over the stable data points 
mean_cal <- csense_cal_711 %>% summarise(mean_bare = mean(as.numeric(co2_ppm_bare[8:11]), na.rm = T),
                       mean_eel = mean(co2_ppm_eelgrass[8:11], na.rm = T),
                       sd_bare = sd(co2_ppm_bare[8:11], na.rm = T),
                       sd_eel = sd(co2_ppm_eelgrass[8:11], na.rm = T))
mean_cal <- mean_cal %>% mutate(date = as.Date("07-11-2023", format = "%m-%d-%y"))

#select only 6/26 cal data 
csense_cal_626  <- csense_raw %>% slice(20028:20036)
#take average over stable data points
mean_cal_626 <- csense_cal_626 %>% summarise(mean_bare = mean(as.numeric(co2_ppm_bare[8:9]), na.rm = T),
                                             mean_eel = mean(co2_ppm_eelgrass[8:9], na.rm = T),
                                             sd_bare = sd(co2_ppm_bare[8:9], na.rm = T),
                                             sd_eel = sd(co2_ppm_eelgrass[8:9], na.rm = T)) 
mean_cal_626<- mean_cal_626 %>% mutate(date = as.Date("06-26-2023", format = "%m-%d-%y"))
mean_cal <- rbind(mean_cal, mean_cal_626)

#select only 6/12 cal data 
csense_cal_612  <- csense_old %>% slice(24046:24056)
#take average over stable data points
view(csense_cal_612)
mean_cal_612 <- csense_cal_612 %>% summarise(mean_bare = mean(as.numeric(co2_ppm_bare[9:11]), na.rm = T),
                                             mean_eel = mean(co2_ppm_eelgrass[9:11], na.rm = T),
                                             sd_bare = sd(co2_ppm_bare[9:11], na.rm = T),
                                             sd_eel = sd(co2_ppm_eelgrass[9:11], na.rm = T)) 
mean_cal_612<- mean_cal_612 %>% mutate(date = as.Date("06-12-2023", format = "%m-%d-%y"))
mean_cal <- rbind(mean_cal, mean_cal_612)

#select only 7/17 cal data 
csense_cal_717  <- csense_raw %>% slice(50075:50088)
#take average over stable data points
view(csense_cal_717)
mean_cal_717 <- csense_cal_717 %>% summarise(mean_bare = mean(as.numeric(co2_ppm_bare[10:14]), na.rm = T),
                                             mean_eel = mean(co2_ppm_eelgrass[10:14], na.rm = T),
                                             sd_bare = sd(co2_ppm_bare[10:14], na.rm = T),
                                             sd_eel = sd(co2_ppm_eelgrass[10:14], na.rm = T)) 
mean_cal_717<- mean_cal_717 %>% mutate(date = as.Date("07-17-2023", format = "%m-%d-%y"))
mean_cal <- rbind(mean_cal, mean_cal_717)

#plot everything 

csense_cal_711 <- csense_cal_711 %>% 
  pivot_longer(cols = contains("co2_ppm"))
csense_cal_626 <- csense_cal_626 %>% 
  pivot_longer(cols = contains("co2_ppm"))
csense_cal_612 <- csense_cal_612 %>% 
  pivot_longer(cols = contains("co2_ppm"))
csense_cal_717 <- csense_cal_717 %>% 
  pivot_longer(cols = contains("co2_ppm"))

p711 <- ggplot(csense_cal_711, aes(datetime, value, color = name)) + 
  geom_line() + labs(title="CO2", x="Time", y="CO2 (ppm)")
ggplotly(p711)
ggsave("csense_cal_711.png",p711)

p626 <- ggplot(csense_cal_626, aes(datetime, value, color = name)) + 
  geom_line() + labs(title="CO2", x="Time", y="CO2 (ppm)")
ggplotly(p626)
ggsave("csense_cal_626.png",p626)

p612 <- ggplot(csense_cal_612, aes(datetime, value, color = name)) + 
  geom_line() + labs(title="CO2", x="Time", y="CO2 (ppm)")
ggplotly(p612)
ggsave("csense_cal_612.png",p612)

p717 <- ggplot(csense_cal_717, aes(datetime, value, color = name)) + 
  geom_line() + labs(title="CO2", x="Time", y="CO2 (ppm)")
ggplotly(p717)
ggsave("csense_cal_717.png",p717)


ggplot(data = mean_cal, aes(date, mean_eel, color = "Eelgrass")) +
  geom_point() + 
  geom_smooth(aes(date, mean_bare, color = "Bare")) +
  geom_point(aes(date, mean_bare, color = "Bare")) + 
  geom_errorbar(aes(ymin = mean_bare - sd_bare, ymax = mean_bare + sd_bare, color = "Bare")) +
  geom_errorbar(aes(ymin = mean_eel - sd_eel, ymax = mean_eel + sd_eel, color = "Eelgrass")) +
  geom_smooth() + 
  labs(title="Csense calibration checks", x="Date", y="CO2 (ppm)") + theme_set(theme_bw()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("blue","darkgreen"))
ggsave("csene_cal_check.png")
