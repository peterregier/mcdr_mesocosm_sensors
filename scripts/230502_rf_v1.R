## Initial random forests (bad ones) for sensors presentation
## Can also serve as a first blush for creating initial models on last year's data

require(pacman)
p_load(tidyverse, ranger, janitor, cowplot, plotly, PNWColors)

theme_set(theme_bw())

## Set directory for presentation
figure_destination = "/Users/regi350/Library/CloudStorage/OneDrive-PNNL/Documents/presentations/2023-04 mCDR Sensors/figures/"

exo_test <- read_csv("data/test/exo_all_data_raw.csv")
co2_test <- read_csv("data/test/csense_all_data_raw.csv") %>% 
  clean_names() %>% 
  rename("pco2_ppm" = aco2ave)

df_raw <- inner_join(exo_test %>% select(datetime, temp_c, depth_m, sal_psu, do_mgl, ph), 
                     co2_test %>% select(datetime, pco2_ppm), 
                     by = "datetime")

## Make a cut
@ggplotly(make_timeseries_plot(pco2_ppm, "pCO2 (ppm)"))

df_trim <- df_raw %>% 
  filter(datetime <= "2021-12-08 16:00")

make_timeseries_plot <- function(var, y_label){
  ggplot(df_trim, aes(datetime, {{var}})) + 
    geom_line() + 
    geom_point(alpha = 0.2) +
    labs(x = "", y = y_label)
}

plot_grid(make_timeseries_plot(pco2_ppm, "pCO2 (ppm)"), 
          make_timeseries_plot(temp_c, "Temp (C)"), 
          make_timeseries_plot(depth_m, "Depth (m)"), 
          make_timeseries_plot(sal_psu, "Sal (PSU)"), 
          make_timeseries_plot(do_mgl, "DO (mg/L)"), 
          make_timeseries_plot(ph, "pH"), 
          ncol = 1)
ggsave(paste0(figure_destination, "mcdr_timeseries.png"), 
       width = 4, height = 6)

rf_co2 <- ranger(pco2_ppm ~ ., data = df_trim %>% select(-datetime), importance = "impurity")

predictors <- as.data.frame(rf_co2$variable.importance) %>% 
                          tibble::rownames_to_column() %>%
                          rename("vi" = `rf_co2$variable.importance`) %>% 
                          as_tibble() %>% 
                          mutate(n_vi = vi / sum(vi), 
                                 dependent = "pCO2")

## set up a color scheme for water quality
pred_vars <- unique(predictors$rowname)
pred_colors <- PNWColors::pnw_palette("Bay", n = length(pred_vars))

ggplot(predictors, aes(n_vi*100, fct_reorder(rowname, n_vi), fill = rowname)) + 
  geom_col(alpha = 0.5, color = "black", show.legend = F) + 
  labs(x = "Importance (%)", y = "Predictor")
ggsave(paste0(figure_destination, "mcdr_feature_importance.png"), 
       width = 4, height = 3)

