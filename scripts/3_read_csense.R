## This script brings in C-Sense data collected on a CR6. Important info: 
## SEVolt1 is in the Bare tank (Tank #5)
## SEVolt2 is in the Eelgrass tank (Tank #3)
##
## 2023-06-20 (updated 8/31/23)
## Peter Regier
##
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## Location of raw EXO datasets
raw_filepath = "data/csense/timeseries"

## Load constants
source("scripts/0_constants.R")


# 2. Read in data --------------------------------------------------------------

## Currently, there should only be one datalogger file. If things get unwieldy,
## that might change, and we may need a list.files %>% map() path like exo

## Set threshold for flagging CO2 based on standard deviation
co2_sd_threshold = 50 #if the sd > threshold, cull

## Helper function
mean_ <- function(var){mean({{var}}, na.rm = T)}
sd_ <- function(var){sd({{var}}, na.rm = T)}

files <- list.files(raw_filepath, pattern = "CR6Series_Table1", full.names = T)

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

df_raw <- files %>% 
  map(read_cr6) %>% 
  bind_rows()


# 3. Make plots to explore data ------------------------------------------------

ggplotly(df_raw %>% 
  select(datetime, contains("co2_ppm")) %>% 
  pivot_longer(cols = -c(contains("datetime"))) %>% 
ggplot(aes(datetime, value, color = name)) + 
  geom_line())

## The time-frame we want is early August till early September
df_trim <- df_raw %>% 
  filter(datetime > "2023-08-04" & 
           datetime < "2023-09-02") 

df_trim %>% 
  select(datetime, contains("co2_ppm")) %>% 
  pivot_longer(cols = -c(contains("datetime"))) %>% 
  ggplot(aes(datetime, value, color = name)) + 
  geom_line() + 
  geom_hline(yintercept = 420)

### Some sort of stuff will need to go in the middle of this script, TBD depending
### on how EXO deployments are structured. But for now, leaving blank and exporting
### so it can be joined with pCO2 data.

# 4. Write out data ------------------------------------------------------------

df_trim <- df_trim %>% 
  mutate(datetime_pdt = as.character(datetime)) %>% 
  relocate(datetime_pdt) %>% 
  select(-datetime)
write_csv(df_trim, "data/231210_csense_trimmed_timeseries_raw.csv")


df_final <- df_raw %>% 
  mutate(datetime_pdt = as.character(datetime)) %>% 
  relocate(datetime_pdt) %>% 
  select(-datetime)
write_csv(df_final, "data/csense_timeseries_raw.csv")
