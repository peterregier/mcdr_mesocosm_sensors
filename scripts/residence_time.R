#Script to calculate the residence times over time from recorded flow rates and the volumes of the tanks
#Also plots DO and temp timeseries over the residence time timeseries to check visually for corelations 
#Finn Roach 

#load required packages
require(pacman)
p_load(tidyverse, 
       parsedate,
       janitor,
       readxl,
       hms,
       lubridate,
       data.table,
       zoo,
       gridExtra)

#code to read in YSI data adapted from kay 

#read in the data from Excel
flow <- read_excel("data/Daily_TankSpotChecks_2023.xlsx") %>% rename("sample_id" = "Sample ID", "t_surface" = "T (C)", "t_deep" = "...4", "salinity_surface" = "Salinity (ppt)", "salinity_deep" = "...6", "pH_surface" = "pH (NBS)", "pH_deep" = "...8", "do_surface" = "DO (mg/L)", "do_deep" = "...10", "time" = "Time Start(PSD)", "date" = "Date", "old_flow" = "Old Flow Rate (L/min)", "new_flow" = "New Flow Rate (L/min)")

#convert chars to numeric from data frame, change sample id to eelgrass densities
flow <- flow %>% select(sample_id, old_flow, new_flow, do_surface, t_surface, date)  %>% drop_na(old_flow) %>% slice(2:nrow(flow))

view(flow)

#make sure timezone is right and select relevant columns
flow <- flow %>% mutate(sample_id = as.numeric(sample_id),
                        old_flow = as.numeric(old_flow),
                        new_flow = as.numeric(new_flow),
                        do_surface = as.numeric(do_surface),
                        t_surface = as.numeric(t_surface),
                        date = as.Date(date))

flow <- flow %>% 
  pivot_longer(c(-date, -sample_id, -do_surface, -t_surface),
               names_to = c("Var", ".value"),
               names_sep = "_")

view(flow)

#plot flow 

ggplot()+ 
  geom_line(data = flow, aes(date, flow))+
  theme_set(theme_bw())+
  facet_wrap(~sample_id)


#read in the volume data from Excel
volume <- read_excel("data/ResidenceTime_eelgrasstanks.xlsx") %>% clean_names() %>% slice(1:8)

volume <- volume %>% rename("sample_id" = "tank") %>% select(sample_id, est_volume_l)

residence <- merge(flow, volume, by = "sample_id")

#create a residence time column 
residence <- residence %>% mutate(restime = est_volume_l/flow)

residence <- residence %>% mutate(restime_hours = restime/60)

view(residence)

#plot residence time and do for each tank 
ggplot() + 
  geom_line(data = na.omit(residence), aes(date, restime_hours, color = "Residence"), linewidth= 1)+
  geom_point(data = na.omit(residence), aes(date, restime_hours, color = "Residence"))+
  geom_line(data = na.omit(residence), aes(date, do_surface*4-12, color = "DO"), linewidth =1)+
  geom_point(data = na.omit(residence), aes(date, do_surface*4-12, color = "DO"))+
  scale_x_date("Date")+
  scale_y_continuous("Residence time (hours)", sec.axis = sec_axis(~(.+12)*0.25, name="Dissolved Oxygen (mg/L)"))+
  theme_set(theme_bw())+
  scale_color_manual(values=c("gold","royalblue"))+
  coord_cartesian(ylim=c(2, 30)) + 
  facet_wrap(~sample_id, ncol= 2)

ggsave("residence_do.png", width = 12, height = 7)

#plot linear regression of residence time against do for each tank 
resnew <- residence %>% filter(grepl("old", Var))
resnew <- residence %>% filter(restime_hours<50)
view(resnew)

ggplot(na.omit(resnew), aes(restime_hours,do_surface))+ 
  geom_point()+
  geom_smooth(method="lm", color = "gold") +
  scale_x_continuous("Residence time (hours)")+
  scale_y_continuous("Dissolved oxygen (mg/L)")+
  facet_wrap(~sample_id, ncol = 2) 

ggsave("residence_do_linear_regressions.png", width = 10, height = 7)

#plot residence time and temp for each tank 
ggplot() + 
  geom_line(data = na.omit(residence), aes(date, restime_hours, color = "Residence"), linewidth= 1)+
  geom_point(data = na.omit(residence), aes(date, restime_hours, color = "Residence"))+
  geom_line(data = na.omit(residence), aes(date, t_surface*4-40, color = "Temperature"), linewidth =1)+
  geom_point(data = na.omit(residence), aes(date, t_surface*4-40, color = "Temperature"))+
  scale_x_date("Date")+
  scale_y_continuous("Residence time (hours)", sec.axis = sec_axis(~(.+40)*0.25, name="Temperature (C)"))+
  theme_set(theme_bw())+
  scale_color_manual(values=c("royalblue","orange"))+
  coord_cartesian(ylim=c(2, 30)) + 
  facet_wrap(~sample_id, ncol= 2)

ggsave("residence_t.png", width = 12, height = 7)

#plot linear regression of residence time against do for each tank 
ggplot(na.omit(resnew), aes(restime_hours,t_surface))+ 
  geom_point()+
  geom_smooth(method="lm", color = "orange") +
  scale_x_continuous("Residence time (hours)")+
  scale_y_continuous("Temperature (C)")+
  facet_wrap(~sample_id, ncol = 2) 

ggsave("residence_t_linear_regressions.png", width = 10, height = 7)
