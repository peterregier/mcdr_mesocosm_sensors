require(pacman)
p_load(tidyverse,
       janitor,
       lubridate,
       gridExtra,
       plotly,
       hms,
       SWMPr,
       rstatix,
       ggpubr)

CTD <- read_delim("data/MCRLdata_CTD_20230529-20230807.csv")  %>%
  clean_names()
CTD <- CTD %>% mutate(time_utc = as.POSIXct(time_utc, tz = "GMT", tryFormats = c("%m/%d/%Y %H:%M:%OS",
                                                                                 "%m/%d/%z %H:%M",
                                                                                 "%m/%d/%Y %H:%M",
                                                                                 "%m-%d-%z %H:%M")))
CTD <- CTD %>% mutate(time_utc = with_tz(time_utc, tz = "America/Los_Angeles"))
CTD <- CTD %>% rename("datetimestamp" = "time_utc", "sal" = "salinity_ppt" , "temp" = "temp_deg_c", "do_mgl" = "do_mg_l") %>% select(datetimestamp, sal, temp, do_mgl)
view(CTD)

met <- read_delim("data/MCRLdata_met_20230529-20230807.csv")  %>%
  clean_names()
met<- met %>% mutate(time_utc = as.POSIXct(time_utc, tz = "GMT", tryFormats = c("%m/%d/%Y %H:%M:%OS",
                                                                                 "%m/%d/%z %H:%M",
                                                                                 "%m/%d/%Y %H:%M",
                                                                                 "%m-%d-%z %H:%M")))
met <- met %>% mutate(time_utc = with_tz(time_utc, tz = "America/Los_Angeles"))
met <- met %>% rename("datetimestamp" = "time_utc", "atemp" = "airtemp_avg_deg_c", "bp" = "baro_pressure_h_pa", "wspd" = "windspeed_avg_m_s") %>% select(datetimestamp, atemp, bp, wspd)

dock <- merge(met, CTD, by = "datetimestamp")
dock["depth"] = as.numeric(1.07)
dock <- data.frame(dock)

exodata <- read_delim("data/exo_timeseries_raw.csv")
view(exodata)
exodata <- exodata %>% mutate(datetimestamp = force_tz(datetime_pdt, tz = "America/Los_Angeles")) %>% rename("temp" = "temp_c", "sal" = "sal_psu", "depth" = "depth_m")
#select the relevant columns and pivot so that there are different columns for the measurements from each tank
exo_bare<- exodata %>% filter(grepl("Bare", tank)) %>% select(datetimestamp, temp, sal, do_mgl, depth)
exo_eel <- exodata %>% filter(grepl("Eel", tank)) %>% select(datetimestamp, temp, sal, do_mgl, depth)

eel_sal_offset <-2.08
bare_sal_offset <- 2.07

exo_bare <- exo_bare %>% mutate(sal = sal + bare_sal_offset)
exo_eel <- exo_eel %>% mutate(sal = sal + eel_sal_offset)
 

exo_bare <- merge(met, exo_bare, by = "datetimestamp", all = TRUE)
exo_eel <- merge(met, exo_eel, by = "datetimestamp", all = TRUE)
exo_bare$sal[8149:13045] <- NA
exo_bare <- data.frame(exo_bare)
exo_eel <- data.frame(exo_eel)


metab <- ecometab(dock,tz = "America/Los_Angeles", lat = 48.079,long = -123.045)
metab_eel <-  ecometab(exo_eel,tz = "America/Los_Angeles", lat = 48.079,long = -123.045)
metab_bare <-  ecometab(exo_bare,tz = "America/Los_Angeles", lat = 48.079,long = -123.045)

ggplot() +
  geom_line(data = metab, aes(date, NEM, color = "Dock")) + 
  geom_line(data = metab_bare, aes(date, NEM, color = "Bare")) + 
  geom_line(data = metab_eel, aes(date, NEM, color = "Eelgrass")) + 
  labs(title="Net metabolism", x="Time", y="NEM (mmol)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("blue","brown","green")) 

ggplot() +
  geom_line(data = metab, aes(date, Pg, color = "Dock")) + 
  geom_line(data = metab_bare, aes(date, Pg, color = "Bare")) + 
  geom_line(data = metab_eel, aes(date, Pg, color = "Eelgrass")) + 
  labs(title="Gross production", x="Time", y="Pg (mmol)") + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_color_manual(values=c("blue","brown","green")) 

metab_eel <- metab_eel %>% rename("eel_NEM" = "NEM", "eel_pg" = "Pg", "eel_rt" = "Rt") %>% select(date, eel_NEM, eel_pg, eel_rt)
metab_bare <- metab_bare %>% rename("bare_NEM" = "NEM", "bare_pg" = "Pg", "bare_rt" = "Rt") %>% select(date, bare_NEM, bare_pg, bare_rt)
metab <- metab %>% rename("dock_NEM" = "NEM", "dock_pg" = "Pg", "dock_rt" = "Rt") %>% select(date, dock_NEM, dock_pg, dock_rt)

metab_tanks <- merge(metab_eel, metab_bare, by = "date") %>% drop_na()
metab_all <- merge(metab_tanks, metab, by = "date") %>% drop_na
view(metab_all)

metab_longer <- metab_all %>% 
  pivot_longer(-date, 
               names_to = c("Environment", ".value"), 
               names_sep = "_") 

my_comparisons = list(c("bare","eel"), c("bare","dock"), c("dock","eel"))

ggboxplot(na.omit(metab_longer), "Environment", "NEM", fill = "Environment", palette = c("forestgreen","royalblue","maroon")) + 
  labs(y = "NEM (mmol m-2 d-1)", title = "Net metabolism") + 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", paired = TRUE) + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))
ggsave("net_metabolism.png", height = 5, width =5 )


ggboxplot(na.omit(metab_longer), "Environment", "rt", fill = "Environment", palette = c("forestgreen","royalblue","maroon")) + 
  labs(y = "Total respiration (mmol m-2 d-1)", title = "Total Respiration") + 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", paired = TRUE) + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12))


ggsave("total_respiration.png", height = 5, width =5 )


ggboxplot(na.omit(metab_longer), "Environment", "pg", fill = "Environment", palette = c("forestgreen","royalblue","maroon")) + 
  labs(y = "Gross production (mmol m-2 d-1)", title = "Gross Production") + 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", paired = TRUE) + 
  theme_set(theme_bw()) + theme(plot.title = element_text(hjust = 0.5, size = 12)) 

ggsave("gross_production.png", height = 5, width = 5)
