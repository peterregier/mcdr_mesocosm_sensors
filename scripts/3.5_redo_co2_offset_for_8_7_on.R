## As of 8/7/23, new calibration offsets have been determined for the C-Sense
## sensors. This means that the factory offsets provided by Turner, which are
## hard-coded into the CR6 code, are no longer applicable. We therefore need to
## redo the co2_ppm calculations, but will keep the original values so we can check
## how the changes in offsets altered the values, and as a reality check that
## values / patterns aren't way, way off the original offset (should be similar)
##
## 2023-08-31
## Peter Regier
##
# ########## #
# ########## #

# 1. Setup ---------------------------------------------------------------------

## Load constants
source("scripts/0_constants.R")


# 2. Remake input datasets -----------------------------------------------------

## Remake datasets
source("scripts/3_read_csense.R")


# 3. First, bring in the file with the calibration and trim --------------------

read_cr6 <- function(file){
  df_raw <- read_delim(file, skip = 1) %>% 
    slice(3:n()) %>% 
    mutate(SEVolt_1 = as.numeric(SEVolt_1), 
           SEVolt_2 = as.numeric(SEVolt_2), 
           SEVoltage_1 = as.numeric(SEVoltage_1), 
           SEVoltage_2 = as.numeric(SEVoltage_2)) %>% 
    mutate(datetime_1min = force_tz(parsedate::parse_date(TIMESTAMP), tzone = common_tz)) %>% 
    mutate(datetime = round_date(datetime_1min, unit = time_interval)) %>% 
    mutate(datetime_raw = as.character(datetime)) %>% 
    #select(datetime, datetime_raw, contains("SEVolt_"))  %>% 
    group_by(datetime) %>% 
    summarize(co2_ppm_bare = mean_(SEVolt_1), 
              co2_ppm_eelgrass = mean_(SEVolt_2),
              sd_ppm_bare = sd_(SEVolt_1), 
              sd_ppm_eelgrass = sd_(SEVolt_2), 
              co2_mv_bare = mean_(SEVoltage_1), 
              co2_mv_eelgrass = mean_(SEVoltage_2),
              sd_mv_bare = sd_(SEVoltage_1), 
              sd_mv_eelgrass = sd_(SEVoltage_2)) 
}

cal_raw <- read_cr6("data/csense/timeseries/CR6Series_Table1_0804_0807.dat") %>% 
  mutate(datetime_pdt = force_tz(datetime, tzone = common_tz))

cal_raw %>% 
  filter(datetime > "2023-08-07 06:00" &
           datetime < "2023-08-07 24:00") %>% 
  ggplot(aes(datetime, co2_ppm_bare)) + geom_point()

# 3. Read in raw C-Sense data and relabel og (and therefore wrong) columns) ----

## First, rename the columns
csense_raw <- read_csv("data/csense_timeseries_raw.csv") %>% 
  force_tz(datetime_pdt, tz = "America/Los_Angeles") %>% 
  rename("og_co2_ppm_bare" = co2_ppm_bare, 
         "og_co2_ppm_eelgrass" = co2_ppm_eelgrass)


csense_raw %>% 
  filter(datetime_pdt > "2023-08-07" &
           datetime_pdt < "2023-08-07 20:00") %>% 
  ggplot(aes(datetime_pdt, og_co2_ppm_bare)) + 
  geom_point()


#make sure the data is in pacific time
csense_raw$datetime <- force_tz(csense_raw$datetime, tz = "America/Los_Angeles")

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
cal_bare <- csense_raw[csense_raw$datetime>bare_start&csense_raw$datetime<bare_end,] %>% select(datetime, SEVoltage_1)
cal_eel <- csense_raw[csense_raw$datetime>eel_start&csense_raw$datetime<eel_end,] %>% select(datetime, SEVoltage_2)

#visualize the calibrations
ggplot(cal_bare, aes(datetime, SEVoltage_1)) + geom_line()
ggplot(cal_eel, aes(datetime, SEVoltage_2)) + geom_line()
view(cal_bare)

#select the most stable part of these curves
cal_bare <- cal_bare %>% slice(14:16)
cal_eel <- cal_eel %>% slice(15:20)










