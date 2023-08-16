require(pacman)
p_load(tidyverse,
       janitor,
       lubridate,
       gridExtra,
       plotly,
       hms,
       seacarb)

exodata <- read_delim("data/exo_timeseries_raw.csv")
exodata <- exodata %>% mutate(p_havg = rowMeans(exodata[, c('p_h1', 'p_h2')]),
                              datetime_pdt = force_tz(datetime_pdt, tz = "America/Los_Angeles")) 
#select the relevant columns and pivot so that there are different columns for the measurements from each tank
exodata <- exodata %>% select(datetime_pdt, temp_c, sal_psu, do_mgl, p_havg, p_h1, p_h2, tank)
exodata <- exodata %>% pivot_wider(names_from = tank, names_sep = "_", values_from = c(temp_c, sal_psu, do_mgl, p_havg, p_h1, p_h2))
exodata <- exodata %>% mutate(time = format(as.POSIXct(datetime_pdt), format = "%H:%M:%S")) 
exodata <- exodata %>% mutate(time = as_hms(time))
#cut out the recent bit of strange salinity data 
#exodata$sal_psu_Bare[8500:nrow(exodata)] <- NA

#CORRECTIONS
#offset for 5/29 to 7/25 based on calibration pre-clean checks  
eel_sal_offset <-2.08
bare_sal_offset <- 2.07
#for eelgrass sonde pH2 preformed better on the pre-clean checks during the 7/25 calibration
#construct column with pH2 up until this point and pHavg after for eelgrass 
#for bare sonde pH1` preformed better on the pre-clean checks during the 7/25 calibration
#construct column with pH1 up until this point and pHavg after for bare
exodata <- exodata[order(exodata$datetime_pdt),]
exodata <- exodata %>% mutate(ph_eel = p_havg_Eelgrass,
                              ph_bare = p_havg_Bare,
                              sal_psu_Bare = c(sal_psu_Bare[1:16644] + bare_sal_offset, sal_psu_Bare[16645:nrow(exodata)]),
                              sal_psu_Eelgrass = c(sal_psu_Eelgrass[1:16644] + eel_sal_offset, sal_psu_Eelgrass[16645:nrow(exodata)])) %>% 
  select(-p_havg_Bare, -p_havg_Eelgrass, -p_h1_Bare, -p_h1_Eelgrass, -p_h2_Bare, -p_h2_Eelgrass)
#plot the timeseries 
exodata$sal_psu_Bare[8568:12708] <- NA
exodata$sal_psu_Bare[15524:16088] <- NA

start1 <- "2023-07-13 00:00"
end1 <- "2023-07-20 00:00"

#remove a c-sense maintenance date 
start2 <- "2023-07-17 09:54"
end2 <- "2023-07-17 10:57"

#reads in the CORRECTED csense data 
csense <-  read_delim("data/csense_timeseries_corrected.csv") %>% 
  select(datetime_raw, co2_ppm_bare, co2_ppm_eelgrass)
#makes sure timezone is in PDT and remove maintenance data 
csense <- csense %>% mutate(datetime_pdt = as.POSIXct(datetime_raw)) 
csense <- csense %>% mutate(datetime_pdt = force_tz(datetime_pdt, tz = "America/Los_Angeles"))
csense[csense$datetime_pdt>start2&csense$datetime_pdt<end2,] <- NA
csense <- csense %>% drop_na()
csense <-  csense[csense$datetime_pdt>start1&csense$datetime_pdt<end1,]

p <- ggplot() + 
  geom_line(data = csense, aes(datetime_pdt, co2_ppm_bare, color = "Bare"))+
  geom_line(data = csense, aes(datetime_pdt, co2_ppm_eelgrass, color = "Eelgrass"))+
  labs(x="Time", y="CO2 (ppm)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("royalblue","forestgreen"))

ggplotly(p)

all <- merge(csense, exodata, by = "datetime_pdt", all = TRUE) %>% filter(sal_psu_Bare > 20) %>% filter(sal_psu_Eelgrass > 20) %>% drop_na()

all <-  all[all$datetime_pdt>start1&all$datetime_pdt<end1,]


## Code borrowed from peter
seacarb_output_bare <- carb(flag = 21, 
                       var1 = all$co2_ppm_bare, 
                       var2 = all$ph_bare, 
                       T = all$temp_c_Bare, 
                       S = all$sal_psu_Bare) %>% 
  mutate(datetime_pdt = all$datetime_pdt, 
         dic_mol_L_bare = DIC * 1.025,
         alk_mol_L_bare = ALK *1.025,
         hco3_mol_L_bare = HCO3 * 1.025,
         co3_mol_L_bare = CO3 * 1.025) %>% # convert kg to L (1.025 kg/L for seawater) 
  select(datetime_pdt,dic_mol_L_bare, alk_mol_L_bare, hco3_mol_L_bare, co3_mol_L_bare) %>% 
  as_tibble()

seacarb_output_eel <- carb(flag = 21, 
                            var1 = all$co2_ppm_eelgrass, 
                            var2 = all$ph_eel, 
                            T = all$temp_c_Eelgrass, 
                            S = all$sal_psu_Eelgrass) %>% 
  mutate(datetime_pdt = all$datetime_pdt, 
         dic_mol_L_eel = DIC * 1.025,
         alk_mol_L_eel = ALK *1.025,
         hco3_mol_L_eel = HCO3 * 1.025,
         co3_mol_L_eel = CO3 * 1.025) %>% # convert kg to L (1.025 kg/L for seawater) 
  select(datetime_pdt,dic_mol_L_eel, alk_mol_L_eel, hco3_mol_L_eel, co3_mol_L_eel) %>% 
  as_tibble()

seacarb_output <- merge(seacarb_output_eel, seacarb_output_bare, by = "datetime_pdt")
carbonate <- merge(all, seacarb_output, by = "datetime_pdt")
carbonate <- carbonate %>% mutate(time = format(as.POSIXct(datetime_pdt), format = "%H:%M:%S")) 
carbonate <- carbonate %>% mutate(time = as_hms(time))

dic <- ggplot() + 
  geom_line(data = carbonate, aes(datetime_pdt, dic_mol_L_bare, color = "Bare"))+
  geom_line(data = carbonate, aes(datetime_pdt, dic_mol_L_eel, color = "Eelgrass"))+
  labs(title="DIC timeseries", x="Time", y="DIC (mol/L)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("royalblue","forestgreen")) 

alk <- ggplot() + 
  geom_line(data = carbonate, aes(datetime_pdt, alk_mol_L_bare, color = "Bare"))+
  geom_line(data = carbonate, aes(datetime_pdt, alk_mol_L_eel, color = "Eelgrass"))+
  labs(title="Alkalinity timeseries", x="Time", y="Alkalinity (mol/L)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("royalblue","forestgreen")) 

co2 <- ggplot() + 
  geom_line(data = carbonate, aes(datetime_pdt, co2_ppm_bare, color = "Bare"))+
  geom_line(data = carbonate, aes(datetime_pdt, co2_ppm_eelgrass, color = "Eelgrass"))+
  labs(title="CO2 timeseries", x="Time", y="CO2 (ppm)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("royalblue","forestgreen"))

ph <- ggplot() + 
  geom_line(data = carbonate, aes(datetime_pdt, ph_bare, color = "Bare"))+
  geom_line(data = carbonate, aes(datetime_pdt, ph_eel, color = "Eelgrass"))+
  labs(title="pH timeseries", x="Time", y="pH") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("royalblue","forestgreen")) 

do<- ggplot() + 
  geom_line(data = carbonate, aes(datetime_pdt, do_mgl_Bare, color = "Bare"))+
  geom_line(data = carbonate, aes(datetime_pdt, do_mgl_Eelgrass, color = "Eelgrass"))+
  labs(title="DO timeseries", x="Time", y="DO (mg/L)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("royalblue","forestgreen")) 

#create dataframes for the daily average plots and graph them 
co2_bare <- carbonate %>% 
  select(time, co2_ppm_bare) %>% 
  group_by(time) %>% 
  summarize(mean = mean(co2_ppm_bare, na.rm = T), 
            sd = sd(co2_ppm_bare, na.rm = T))
co2_eel <- carbonate %>% 
  select(time, co2_ppm_eelgrass) %>% 
  group_by(time) %>% 
  summarize(mean = mean(co2_ppm_eelgrass, na.rm = T), 
            sd = sd(co2_ppm_eelgrass, na.rm = T))
ph_bare <- carbonate %>% 
  select(time,ph_bare) %>% 
  group_by(time) %>% 
  summarize(mean = mean(ph_bare, na.rm = T), 
            sd = sd(ph_bare, na.rm = T))
ph_eel <- carbonate %>% 
  select(time,ph_eel) %>% 
  group_by(time) %>% 
  summarize(mean = mean(ph_eel, na.rm = T), 
            sd = sd(ph_eel, na.rm = T))
dic_bare <- carbonate %>% 
  select(time, dic_mol_L_bare) %>% 
  group_by(time) %>% 
  summarize(mean = mean(dic_mol_L_bare, na.rm = T), 
            sd = sd(dic_mol_L_bare, na.rm = T))
dic_eel <- carbonate %>% 
  select(time, dic_mol_L_eel) %>% 
  group_by(time) %>% 
  summarize(mean = mean(dic_mol_L_eel, na.rm = T), 
            sd = sd(dic_mol_L_eel, na.rm = T))
alk_bare <- carbonate %>% 
  select(time, alk_mol_L_bare) %>% 
  group_by(time) %>% 
  summarize(mean = mean(alk_mol_L_bare, na.rm = T), 
            sd = sd(alk_mol_L_bare, na.rm = T))
alk_eel <- carbonate %>% 
  select(time, alk_mol_L_eel) %>% 
  group_by(time) %>% 
  summarize(mean = mean(alk_mol_L_eel, na.rm = T), 
            sd = sd(alk_mol_L_eel, na.rm = T))
do_bare <- carbonate %>% 
  select(time, do_mgl_Bare) %>% 
  group_by(time) %>% 
  summarize(mean = mean(do_mgl_Bare, na.rm = T), 
            sd = sd(do_mgl_Bare, na.rm = T))
do_eel <- carbonate %>% 
  select(time, do_mgl_Eelgrass) %>% 
  group_by(time) %>% 
  summarize(mean = mean(do_mgl_Eelgrass, na.rm = T), 
            sd = sd(do_mgl_Eelgrass, na.rm = T))

ph_avg <- ggplot(data = ph_bare, aes(time,mean, color = "Bare")) + 
  geom_smooth(data = ph_eel, aes(time, mean, color = "Eelgrass")) + 
  geom_errorbar(data = ph_bare, aes(ymin = mean - sd, ymax = mean + sd, color = "Bare"), alpha = 0.2) +
  geom_errorbar(data = ph_eel, aes(ymin = mean - sd, ymax = mean + sd, color = "Eelgrass"), alpha = 0.2) +
  geom_smooth() + labs(title="Mean daily pH", x="Time", y="Mean pH)") + theme_set(theme_bw()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("royalblue","forestgreen"))
co2_avg <- ggplot(data = co2_bare, aes(time,mean, color = "Bare")) + 
  geom_smooth(data = co2_eel, aes(time, mean, color = "Eelgrass")) + 
  geom_errorbar(data = co2_bare, aes(ymin = mean - sd, ymax = mean + sd, color = "Bare"), alpha = 0.2) +
  geom_errorbar(data = co2_eel, aes(ymin = mean - sd, ymax = mean + sd, color = "Eelgrass"), alpha = 0.2) +
  geom_smooth() + labs(title="Mean daily CO2", x="Time", y="Mean CO2 (ppm))") + theme_set(theme_bw()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("royalblue","forestgreen"))
dic_avg <- ggplot(data = dic_bare, aes(time,mean, color = "Bare")) + 
  geom_smooth(data = dic_eel, aes(time, mean, color = "Eelgrass")) + 
  geom_errorbar(data = dic_bare, aes(ymin = mean - sd, ymax = mean + sd, color = "Bare"), alpha = 0.2) +
  geom_errorbar(data = dic_eel, aes(ymin = mean - sd, ymax = mean + sd, color = "Eelgrass"), alpha = 0.2) +
  geom_smooth() + labs(title="Mean daily DIC", x="Time", y="Mean DIC (mol/L))") + theme_set(theme_bw()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("royalblue","forestgreen"))
alk_avg <- ggplot(data = alk_bare, aes(time,mean, color = "Bare")) + 
  geom_smooth(data = alk_eel, aes(time, mean, color = "Eelgrass")) + 
  geom_errorbar(data = alk_bare, aes(ymin = mean - sd, ymax = mean + sd, color = "Bare"), alpha = 0.2) +
  geom_errorbar(data = alk_eel, aes(ymin = mean - sd, ymax = mean + sd, color = "Eelgrass"), alpha = 0.2) +
  geom_smooth() + labs(title="Mean daily Alkalinity", x="Time", y="Alkalinity (mol/L))") + theme_set(theme_bw()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("royalblue","forestgreen"))
do_avg <- ggplot(data = do_bare, aes(time,mean, color = "Bare")) + 
  geom_smooth(data = do_eel, aes(time, mean, color = "Eelgrass")) + 
  geom_errorbar(data = do_bare, aes(ymin = mean - sd, ymax = mean + sd, color = "Bare"), alpha = 0.2) +
  geom_errorbar(data = do_eel, aes(ymin = mean - sd, ymax = mean + sd, color = "Eelgrass"), alpha = 0.2) +
  geom_smooth() + labs(title="Mean daily DO", x="Time", y="DO (mg/L)") + theme_set(theme_bw()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + scale_color_manual(values=c("royalblue","forestgreen"))

co2_plot<- grid.arrange(co2,co2_avg, widths = c(1, 0.4))
ph_plot <- grid.arrange(ph,ph_avg, widths = c(1, 0.4))
dic_plot <- grid.arrange(dic,dic_avg, widths = c(1, 0.4))
alk_plot <- grid.arrange(alk,alk_avg, widths = c(1, 0.4))
do_plot <- grid.arrange(do,do_avg, widths = c(1, 0.4))
ggsave("do_plot.png", do_plot, height = 3.5, width = 16)

#plot everything on one grid 
carbonate_plots <- grid.arrange(co2_plot, ph_plot, dic_plot, alk_plot, nrow = 4)
ggsave("carbonate_plots.png", carbonate_plots, height = 14, width = 16)

csense %>% 
  select(datetime_pdt, contains("co2")) %>%
  pivot_longer(cols = -c(contains("datetime"))) %>% 
  ggplot(aes(datetime_pdt, value, color = name)) + 
  geom_line() 

badstart <- "2023-06-18 10:00"
badend <- "2023-06-20 00:00"

goodstart <- "2023-08-04 10:00"
goodend <- "2023-08-06 00:00"

bad_csense <- csense[csense$datetime_pdt>badstart&csense$datetime_pdt<badend,]
good_csense <- csense[csense$datetime_pdt>goodstart&csense$datetime_pdt<goodend,]

badco2 <- ggplot() + 
  geom_line(data = bad_csense, aes(datetime_pdt, co2_ppm_bare, color = "Bare"))+
  geom_line(data = bad_csense, aes(datetime_pdt, co2_ppm_eelgrass, color = "Eelgrass"))+
  labs(title="CO2 timeseries", x="Time", y="CO2 (ppm)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("royalblue","forestgreen"))

goodco2 <- ggplot() + 
  geom_line(data = good_csense, aes(datetime_pdt, co2_ppm_bare, color = "Bare"))+
  geom_line(data = good_csense, aes(datetime_pdt, co2_ppm_eelgrass, color = "Eelgrass"))+
  labs(x="Time", y="CO2 (ppm)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("royalblue","forestgreen"))

co2_comparison<- grid.arrange(badco2,goodco2, nrow = 2)
ggsave("co2_comparison.png", co2_comparison, width =6, height = 5)
