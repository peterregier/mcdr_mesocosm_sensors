#Finn Roach
#Code to plot a full day timeseries and do a linear regression against csense data from the nearest timestamps 

require(pacman)
p_load(tidyverse, 
       parsedate,
       janitor,
       readxl,
       hms,
       lubridate,
       data.table,
       ggpubr)

#reads in the csense data 
csense <-  read_delim("data/csense_timeseries_corrected.csv") %>% 
  select(datetime_pdt, co2_ppm_bare, co2_ppm_eelgrass)
csense <- csense %>% mutate(datetime_pdt = as.POSIXct(datetime_pdt)) 
csense <- csense %>% mutate(datetime_pdt = force_tz(datetime_pdt, tz = "America/Los_Angeles"))
csense <- csense %>% rename(datetime = datetime_pdt)


#load in timeseries sheet
timeseries <- read_excel("data/Bi-weekly_Tank_Sampling_2023.xlsx",4) %>% 
  select(contains("Sample ID"), contains("CO2 mean"), contains("CO2 stdev"), contains("Date"), contains("Time Collected (PSD)")) %>%
  clean_names() %>% slice(75:90)
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
both <- merge(grabs, csense, by = "datetime", all = TRUE) %>% slice(23462:23558)
view(both)
#plot the full day timeseries 
ggplot()+ 
  geom_point(data = both, aes(datetime, co2_5, color = "Bare"))+
  geom_smooth(data = both, aes(datetime, co2_5, color="Bare", linetype = "Bare"), method="loess")+
  geom_point(data = both, aes(datetime, co2_3, color = "Eelgrass"))+
  geom_smooth(data = both, aes(datetime, co2_3, color="Eelgrass", linetype = "Eelgrass"), method="loess")+
  geom_smooth(data = both, aes(datetime, co2_ppm_bare, color="C-sense bare", linetype = "C-sense bare"), method="loess")+
  geom_smooth(data = both, aes(datetime, co2_ppm_eelgrass, color= "C-sense eelgrass", linetype = "C-sense eelgrass"), method="loess")+
  labs(title="CO2 timeseries", x="Time", y="CO2 (ppm)") + 
  scale_linetype_manual(name = "Legend", values = c("Bare" = "solid", "C-sense bare" = "dashed", "Eelgrass" = "solid", "C-sense eelgrass" = "dashed"))+
  scale_color_manual(name = "Legend", values=c("Bare" = "royalblue", "C-sense bare" = "royalblue", "Eelgrass" = "forestgreen", "C-sense eelgrass" = "forestgreen"))+ 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))
ggsave("full_day_timeseries.png", width = 7, height = 5)

#generate time matched dataframe to do linear regression of grab samples against csense 

#converts data frames to data tables
grabs <- data.table(grabs)
csense <- data.table(csense)   

#sets keys to align timecodes 
setkey(grabs, "datetime")
setkey(csense, "datetime")

#aligns the sensor and grab sample data by nearest timecode 
aligned <- csense[grabs, roll = TRUE]

aligned_5 <- aligned %>% drop_na(co2_5)  %>% select(datetime, co2_ppm_bare, co2_5)
aligned_3 <- aligned %>% drop_na(co2_3)  %>% select(datetime, co2_ppm_eelgrass, co2_3)

aligned_5 <- aligned_5 %>% rename("co2"="co2_5", "co2_ppm"="co2_ppm_bare")
aligned_3 <- aligned_3 %>% rename("co2"="co2_3", "co2_ppm"="co2_ppm_eelgrass")

stacked <- rbind(aligned_5, aligned_3)

#view(stacked)

#stacked <- stacked[!12,]

#head(aligned_5)

#plot everything 

p_bare <- ggplot(aligned_5, aes(co2_ppm, co2))+ 
  geom_point()+
  geom_smooth(method="lm",color="royalblue")+
  geom_abline(slope = 1, intercept = 0, alpha = 0.2, color="royalblue")+
  labs(title="CO2 bare tank ", x="CSense CO2 (ppm)", y="Grab Sample CO2 (ppm)") +
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12)) + ylim(500,800) + 
  stat_regline_equation(label.x = 500, label.y = 700) + stat_cor(aes(label=after_stat(rr.label)), label.y=735, label.x=500)

ggsave("co2_bare_full_day.png",width = 6, height = 6)
summary(lm(co2_ppm~co2, data=aligned_5))

p_eel <- ggplot(aligned_3, aes(co2_ppm, co2))+ 
  geom_point()+
  geom_smooth(method="lm",color="forestgreen")+
  geom_abline(slope = 1, intercept = 0, alpha = 0.2, color="forestgreen")+
  labs(title="CO2 eelgrass tank", x="CSense CO2 (ppm)", y="Grab Sample CO2 (ppm)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+ 
  stat_regline_equation(label.x = 500, label.y = 700) + stat_cor(aes(label=after_stat(rr.label)), label.y=750, label.x=500)

ggsave("co2_eelgrass_full_day.png", width = 4, height = 4)
summary(lm(co2_ppm~co2, data=aligned_3))

lin_regression_co2 <- grid.arrange(p_bare, p_eel, nrow = 2)
ggsave("lin_regression_co2.png", lin_regression_co2, width =4, height = 5)

ggplot(stacked, aes(co2_ppm, co2))+
  geom_point()+ 
  geom_smooth(method="lm", color="turquoise")+ 
  labs(title="CO2", x="CSense CO2 Conc (ppm)", y="Grab Sample CO2 Conc (ppm)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))

ggsave("co2.png",width = 4, height = 4)
summary (lm(co2_ppm~co2, data = stacked))
