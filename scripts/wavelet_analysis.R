#Finn Roach 

require(pacman)
p_load(tidyverse,
       janitor,
       lubridate,
       gridExtra,
       plotly,
       WaveletComp,
       hms,
       viridis,
       scales)


## ADD SOME WAVELET ANALYSIS FOR CO2 DATA FROM 7/11 TO 7/25!!! 

CTD <- read_delim("data/MCRLdata_CTD_20230529-20230807.csv")  %>%
  clean_names()
CTD <- CTD %>% mutate(time_utc = as.POSIXct(time_utc, tz = "GMT", tryFormats = c("%m/%d/%Y %H:%M:%OS",
                                                                                 "%m/%d/%z %H:%M",
                                                                                 "%m/%d/%Y %H:%M",
                                                                                 "%m-%d-%z %H:%M")))
CTD <- CTD %>% mutate(time_utc = with_tz(time_utc, tz = "America/Los_Angeles"))
CTD <- CTD %>% rename("datetime" = "time_utc")
CTD <- CTD %>% mutate("date" = as.Date(trunc(datetime, 'days')))
#remove weird blip 
CTD[12910,] <- NA
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
exodata <- exodata %>% mutate("date" = as.Date(trunc(datetime_pdt, 'days'))) %>% rename("datetime" = "datetime_pdt")
#cut out the recent bit of strange salinity data 
#exodata$sal_psu_Bare[8500:nrow(exodata)] <- NA

#reads in the csense data 
csense <-  read_delim("data/csense_timeseries_raw.csv") %>% 
  select(datetime_pdt, co2_ppm_bare, co2_ppm_eelgrass) %>% slice(13160:n()) %>% drop_na()
#makes sure timezone is in PDT 
csense$datetime_pdt <- force_tz(csense$datetime_pdt, tz = "America/Los_Angeles") 
csense <- csense %>% mutate("date" = as.Date(trunc(datetime_pdt, 'days'))) %>% rename("datetime" = "datetime_pdt") %>% select(date, co2_ppm_bare, co2_ppm_eelgrass, datetime)

#generate time matched dataframe to do linear regression of grab samples against csense 
all <- merge(exodata, CTD, by = c("datetime", "date"))
exodata <- exodata %>% select(date, datetime, temp_c_Bare, temp_c_Eelgrass, sal_psu_Bare, sal_psu_Eelgrass, do_mgl_Bare, do_mgl_Eelgrass)  %>%  drop_na()
CTD <- CTD %>% select(date, datetime, temp_deg_c, do_mg_l, salinity_ppt) %>%  drop_na() %>% mutate(temp_deg_c = as.numeric(temp_deg_c),
                                                                                                                             salinity_ppt = as.numeric(salinity_ppt),
                                                                                                                             do_mg_l = as.numeric(do_mg_l))
all <- all %>% select(date, datetime, temp_c_Bare, temp_c_Eelgrass, sal_psu_Bare, sal_psu_Eelgrass, do_mgl_Bare, do_mgl_Eelgrass,
                      temp_deg_c, do_mg_l, salinity_ppt) %>% drop_na()

#code adpated from peter! 

wc_analysis <- function(x,st,en,col) {
  df<-x
  df <- subset(df,datetime>st&datetime<en)
  wc <- analyze.wavelet(my.data=df, my.series = col, dt = 1/12)
  return(wc)
}

wc_to_avg <- function(x){
  averages <- data.frame(period=x$Period,power=x$Power.xy.avg,pval=x$Power.x.avg.pval)
}

wc_to_df <- function(x,datelist){
  df <- reshape2::melt(x$Power)
  names(df) <- c("y","index","power")
  df$period <- x$Period[df$y]
  df$datetime <- as.POSIXct(datelist)[df$index]
  return(df)
}

view(exodata)
view(CTD)

start1 <- "2023-06-013 00:00"
end1 <- "2023-08-07 00:00"

datelist_exo <- as.POSIXct(exodata$datetime[exodata$datetime>start1&exodata$datetime<end1])
datelist_CTD <- as.POSIXct(CTD$datetime[CTD$datetime>start1&CTD$datetime<end1])

temp_eel <- wc_analysis(exodata,start1,end1,"temp_c_Eelgrass")
temp_bare <- wc_analysis(exodata,start1,end1,"temp_c_Bare")
temp_CTD <-  wc_analysis(CTD,start1,end1,"temp_deg_c")

do_eel <- wc_analysis(exodata,start1,end1,"do_mgl_Eelgrass")
do_bare <- wc_analysis(exodata,start1,end1,"do_mgl_Bare")
do_CTD <-  wc_analysis(CTD,start1,end1,"do_mg_l")

sal_eel <- wc_analysis(exodata,start1,end1,"sal_psu_Eelgrass")
sal_bare <- wc_analysis(exodata,start1,end1,"sal_psu_Bare")
sal_CTD <-  wc_analysis(CTD,start1,end1,"salinity_ppt")

df_temp_eel <- wc_to_df(temp_eel,datelist_exo)
df_temp_bare <- wc_to_df(temp_bare,datelist_exo)
df_temp_CTD <- wc_to_df(temp_CTD,datelist_CTD)

df_do_eel <- wc_to_df(do_eel,datelist_exo)
df_do_bare <- wc_to_df(do_bare,datelist_exo)
df_do_CTD <- wc_to_df(do_CTD,datelist_CTD)

df_sal_eel <- wc_to_df(sal_eel,datelist_exo)
df_sal_bare <- wc_to_df(sal_bare,datelist_exo)
df_sal_CTD <- wc_to_df(sal_CTD,datelist_CTD)


#Now, make wavelets
viridis_code <- "D"
n=20

p_temp_eel <- ggplot(df_temp_eel,aes(x=datetime,y=period,z=power)) + 
  geom_contour_filled(bins=n,show.legend=F) + scale_fill_viridis_d(option=viridis_code,begin=0.01) +
  scale_y_continuous(breaks=c(12,24),labels=c("tidal","daily"),trans="log10",expand = c(-0.05,0), limits = c(5,91)) + 
  scale_x_datetime(expand = c(0,0),date_breaks="10 days",labels=date_format("%b %d")) + labs(x="Date",y="Cycle", title = "Eelgrass tank temperature") + 
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave("wavelet_temp_eel.png", width = 5, height = 5)

p_temp_CTD <- ggplot(df_temp_CTD,aes(x=datetime,y=period,z=power)) + 
  geom_contour_filled(bins=n,show.legend=F) + scale_fill_viridis_d(option=viridis_code,begin=0.01, end = 1) +
  scale_y_continuous(breaks=c(12,24),labels=c("tidal","daily"),trans="log10", expand = c(-0.05,0), limits = c(3,70)) + 
  scale_x_datetime(expand = c(0,0),date_breaks="10 days",labels=date_format("%b %d")) + labs(x="Date",y="Cycle", title = "Dock temperature") + 
  theme(plot.title = element_text(hjust = 0.5)) 
p_temp_CTD
ggsave("wavelet_temp_CTD.png", width = 5, height = 5)

p_temp_bare <- ggplot(df_temp_bare,aes(x=datetime,y=period,z=power)) + 
  geom_contour_filled(bins=n,show.legend=F) + scale_fill_viridis_d(option=viridis_code,begin=0.01) +
  scale_y_continuous(breaks=c(12,24),labels=c("tidal","daily"),trans="log10",expand = c(-0.05,0), limits = c(5,91)) + 
  scale_x_datetime(expand = c(0,0),date_breaks="10 days",labels=date_format("%b %d")) + labs(x="Date",y="Cycle", title = "Bare tank temperature") + 
  theme(plot.title = element_text(hjust = 0.5)) 
p_temp_bare
ggsave("wavelet_temp_bare.png", width = 5, height = 5)

p_do_eel <- ggplot(df_do_eel,aes(x=datetime,y=period,z=power)) + 
  geom_contour_filled(bins=n,show.legend=F) + scale_fill_viridis_d(option=viridis_code,begin=0.01) +
  scale_y_continuous(breaks=c(12,24),labels=c("tidal","daily"),trans="log10",expand = c(-0.05,0), limits = c(5,91)) + 
  scale_x_datetime(expand = c(0,0),date_breaks="10 days",labels=date_format("%b %d")) + labs(x="Date",y="Cycle", title = "Eelgrass tank DO") + 
  theme(plot.title = element_text(hjust = 0.5)) 
p_do_eel
ggsave("wavelet_do_eel.png", width = 5, height = 5)

p_do_CTD <- ggplot(df_do_CTD,aes(x=datetime,y=period,z=power)) + 
  geom_contour_filled(bins=n,show.legend=F) + scale_fill_viridis_d(option=viridis_code,begin=0.01, end = 1) +
  scale_y_continuous(breaks=c(12,24),labels=c("tidal","daily"),trans="log10", expand = c(-0.05,0), limits = c(3,70)) + 
  scale_x_datetime(expand = c(0,0),date_breaks="10 days",labels=date_format("%b %d")) + labs(x="Date",y="Cycle", title = "Dock DO") + 
  theme(plot.title = element_text(hjust = 0.5)) 
p_do_CTD
ggsave("wavelet_do_CTD.png", width = 5, height = 5)

p_do_bare <- ggplot(df_do_bare,aes(x=datetime,y=period,z=power)) + 
  geom_contour_filled(bins=n,show.legend=F) + scale_fill_viridis_d(option=viridis_code,begin=0.01) +
  scale_y_continuous(breaks=c(12,24),labels=c("tidal","daily"),trans="log10",expand = c(-0.05,0), limits = c(5,91)) + 
  scale_x_datetime(expand = c(0,0),date_breaks="10 days",labels=date_format("%b %d")) + labs(x="Date",y="Cycle", title = "Bare tank DO") + 
  theme(plot.title = element_text(hjust = 0.5)) 
p_do_bare
ggsave("wavelet_do_bare.png", width = 5, height = 5)

#old wavelet analysis and visualization code 
co2_bare <- analyze.wavelet(my.data = csense,  my.series = "co2_ppm_bare", dt = 1/12)
co2_eel <- analyze.wavelet(my.data = csense,  my.series = "co2_ppm_eelgrass", dt = 1/12)
wt.image(co2_bare)
wt.image(co2_eel)
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
sal_eel <- analyze.coherency(my.data = as.data.frame(all), my.pair = c("salinity_ppt", "sal_psu_Eelgrass"), dt = 1/12)
wc.image(sal_eel)
temp_eel <- analyze.coherency(my.data = as.data.frame(all), my.pair = c("temp_deg_c", "temp_c_Eelgrass"), dt = 1/12)
wc.image(temp_eel)
