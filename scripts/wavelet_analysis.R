#Finn Roach 

require(pacman)
p_load(tidyverse,
       janitor,
       lubridate,
       gridExtra,
       plotly,
       WaveletComp,
       hms)

CTD <- read_delim("data/CTD_20230701_dock.csv")  %>%
  clean_names()
CTD <- CTD %>% mutate(time_utc = as.POSIXct(time_utc, tz = "GMT", tryFormats = c("%m/%d/%Y %H:%M:%OS",
                                                                                 "%m/%d/%z %H:%M",
                                                                                 "%m/%d/%Y %H:%M",
                                                                                 "%m-%d-%z %H:%M")))
CTD <- CTD %>% mutate(time_utc = with_tz(time_utc, tz = "America/Los_Angeles"))
CTD <- CTD %>% rename("time_pdt" = "time_utc")
CTD <- CTD %>% mutate("date" = as.Date(trunc(time_pdt, 'days')))
view(CTD)

#read in the dock data and convert to pdt 
#tide <- read_delim("data/TideGauge_20230601.csv")  %>%
  #clean_names()
#tide <- tide %>% mutate(time = as.POSIXct(time, tz = "GMT", tryFormats = c("%m/%d/%Y %H:%M:%OS",
                                                                                 #"%m/%d/%z %H:%M",
                                                                                 #"%m/%d/%Y %H:%M",
                                                                                 #"%m-%d-%z %H:%M")))

#tide <- tide %>% mutate(time = as.POSIXct(time, format = "%Y/%m/%d %H:%M:%S"))
#tide <- tide %>% mutate(time = with_tz(time, tz = "America/Los_Angeles"))
#tide <- tide %>% rename("time_pdt" = "time", "water_level" = "water_level_m_navd88")
#tide <- tide %>% mutate("date" = as.Date(trunc(time_pdt, 'days')))
#view(tide)
#CTD <- merge(tide, CTD, by = c("time_pdt", "date"))

exodata <- read_delim("data/exo_timeseries_raw.csv")
exodata <- exodata %>% mutate(p_havg = rowMeans(exodata[, c('p_h1', 'p_h2')]),
                              datetime_pdt = force_tz(datetime_pdt, tz = "America/Los_Angeles")) 
#select the relevant columns and pivot so that there are different columns for the measurements from each tank
exodata <- exodata %>% select(datetime_pdt, temp_c, sal_psu, do_mgl, p_havg, tank)
exodata <- exodata %>% pivot_wider(names_from = tank, names_sep = "_", values_from = c(temp_c, sal_psu, do_mgl, p_havg))

exodata <- exodata %>% mutate("date" = as.Date(trunc(datetime_pdt, 'days'))) %>% rename("time_pdt" = "datetime_pdt")


#cut out the recent bit of strange salinity data 
exodata$sal_psu_Bare[8500:nrow(exodata)] <- NA

view(exodata)
view(CTD)


all <- merge(exodata, CTD, by = c("time_pdt", "date"))

view(all)

exodata <- exodata %>% select(date, temp_c_Bare, temp_c_Eelgrass, sal_psu_Bare, sal_psu_Eelgrass, do_mgl_Bare, do_mgl_Eelgrass)  %>%  drop_na()
CTD <- CTD %>% select(date, temp_deg_c, do_mg_l, salinity_ppt) %>%  drop_na() %>% mutate(temp_deg_c = as.numeric(temp_deg_c),
                                                                                                                             salinity_ppt = as.numeric(salinity_ppt),
                                                                                                                             do_mg_l = as.numeric(do_mg_l))
all <- all %>% select(date, temp_c_Bare, temp_c_Eelgrass, sal_psu_Bare, sal_psu_Eelgrass, do_mgl_Bare, do_mgl_Eelgrass,
                      temp_deg_c, do_mg_l, salinity_ppt) %>% drop_na()
  
CTD_temp <- analyze.wavelet(my.data = CTD,  my.series = "temp_deg_c", dt = 1/12)
CTD_sal <- analyze.wavelet(my.data = CTD,  my.series = "salinity_ppt", dt = 1/12)
CTD_do <- analyze.wavelet(my.data = CTD,  my.series = "do_mg_l", dt = 1/12)
CTD_tide <- analyze.wavelet(my.data = CTD,  my.series = "water_level", dt = 1/12)
wt.image(CTD_temp)
wt.image(CTD_sal)
wt.image(CTD_do)
wt.image(CTD_tide)

exo_temp_eel <- analyze.wavelet(my.data = exodata,  my.series = "temp_c_Eelgrass", dt = 1/12)
exo_sal_eel <- analyze.wavelet(my.data = exodata,  my.series = "sal_psu_Eelgrass", dt = 1/12)
exo_do_eel <- analyze.wavelet(my.data = exodata,  my.series = "do_mgl_Eelgrass", dt = 1/12)
wt.image(exo_temp_eel)
wt.image(exo_sal_eel)
wt.image(exo_do_eel)

exo_temp_bare <- analyze.wavelet(my.data = exodata,  my.series = "temp_c_Bare", dt = 1/12)
exo_sal_bare <- analyze.wavelet(my.data = exodata,  my.series = "sal_psu_Bare", dt = 1/12)
exo_do_bare <- analyze.wavelet(my.data = exodata,  my.series = "do_mgl_Bare", dt = 1/12)
wt.image(exo_temp_bare)
wt.image(exo_sal_bare)
wt.image(exo_do_bare)

do_eel <- analyze.coherency(my.data = as.data.frame(all), my.pair = c("do_mg_l", "do_mgl_Eelgrass"), dt = 1/12)
wc.image(do_eel)

sal_bare <- analyze.coherency(my.data = as.data.frame(all), my.pair = c("salinity_ppt", "sal_psu_Bare"), dt = 1/12)
wc.image(sal_bare)
