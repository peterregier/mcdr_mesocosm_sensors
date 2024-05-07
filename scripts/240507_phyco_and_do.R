## Plot dock data for phycocyanin and DO to see if there are relationships that 
## might explain why we such low DO values
## 
## Peter Regier
## 2024-05-07
##
# ########## #
# ########## #

# 1. Setup

require(pacman)
p_load(tidyverse, 
       janitor, 
       cowplot,
       readxl)

theme_set(theme_bw())

read_excel_sheet <- function(sheet_name) {
  
  filepath <- "data/MCRLData_2023_2024_043024.xlsx"
  
  data <- read_excel(filepath, sheet = sheet_name) %>% 
    clean_names() %>% 
    mutate(time_utc_5min = round_date(time_utc, "5 min"))
  return(data)
}

ctd <- read_excel_sheet("CTD") %>% 
                   dplyr::select(time_utc_5min, do_mg_l, qc_do)

ctd %>% 
  drop_na() %>% 
  ggplot(aes(time_utc_5min, do_mg_l)) + 
  geom_point()

df <- full_join(read_excel_sheet("CTD") %>% 
  dplyr::select(time_utc_5min, temp_deg_c, do_mg_l, salinity_ppt), 
  read_excel_sheet("Fluorometer")  %>% 
  dplyr::select(time_utc_5min, phycoerythrin_ppb, qc_phycoerythrin), 
  by = "time_utc_5min") %>% 
  mutate(datetime_pdt = force_tz(time_utc_5min, tzone = "Etc/GMT+7"))

df_trim <- df %>% 
  filter(datetime_pdt > "2023-07-01" & 
           datetime_pdt < "2023-08-01")

plot_grid(ggplot(df_trim, aes(datetime_pdt, temp_deg_c)) + 
                   geom_line(), 
          ggplot(df_trim, aes(datetime_pdt, salinity_ppt)) + 
            geom_line(), 
          ggplot(df_trim, aes(datetime_pdt, do_mg_l)) + 
            geom_line(), 
          ggplot(df_trim, aes(datetime_pdt, phycoerythrin_ppb)) + 
            geom_line(), 
          ncol = 1)
ggsave("figures/phyco_v_do/phyco_ctd_timeseries.png", 
       width = 6, height = 10)

df_trim2 <- df_trim %>% 
  filter(datetime_pdt > "2023-07-25" & 
           datetime_pdt < "2023-07-30") 

plot_grid(ggplot(df_trim2, aes(datetime_pdt, temp_deg_c)) + 
            geom_vline(xintercept = as.POSIXct("2023-07-28 18:40:00", tz = "Etc/GMT+7"), 
                       color = "gray", linetype = "dashed") + 
            geom_line(), 
          ggplot(df_trim2, aes(datetime_pdt, salinity_ppt)) + 
            geom_vline(xintercept = as.POSIXct("2023-07-28 18:40:00", tz = "Etc/GMT+7"), 
                       color = "gray", linetype = "dashed") + 
            geom_line(), 
          ggplot(df_trim2, aes(datetime_pdt, do_mg_l)) + 
            geom_vline(xintercept = as.POSIXct("2023-07-28 18:40:00", tz = "Etc/GMT+7"), 
                       color = "gray", linetype = "dashed") + 
            geom_line(), 
          ggplot(df_trim2, aes(datetime_pdt, phycoerythrin_ppb)) + 
            geom_vline(xintercept = as.POSIXct("2023-07-28 18:40:00", tz = "Etc/GMT+7"), 
                       color = "gray", linetype = "dashed") + 
            geom_line(), 
          ncol = 1)
ggsave("figures/phyco_v_do/phyco_ctd_maintenance.png", 
       width = 6, height = 10)


plot_grid(ggplot(df_trim2, aes(datetime_pdt, do_mg_l)) + 
            geom_line(), 
          ggplot(df_trim2, aes(datetime_pdt, phycoerythrin_ppb)) + 
            geom_line(), 
          ncol = 1)

%>% 
  mutate(time_of_day = hour(time_utc_5min)) %>% 
  group_by(time_of_day) %>% 
  
  ggplot(aes(time_utc_5min, phycoerythrin_ppb)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()


