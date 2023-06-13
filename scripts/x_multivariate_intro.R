## This script is (hopefully) a light introduction to multivariate statistics!
## The goal of these analyses is to understand how different measurements relate
## to each other. The simplest relationships to find and explain is a linear
## relationship, which we observe as correlation between two variables. Our first
## step will be exploring individual relationships and also correlation matrices
## to look for any useful relationships. If you find an interesting story, you 
## may not need anything more complicated than correlations. If you want to dig 
## deeper, I'd suggest looking into principal component analysis (esp using
## ggbiplot) - I can walk making and interpreting if you want. Also, multiple
## linear regression, which is a really powerful tool, but also much more complicated
## to do right. 
##
## 2022-06-14 (Updated 5/13/23)
## Peter Regier
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load useful packages
require(pacman)
p_load(tidyverse, ## keep things tidy and load ggplot2
       ggpubr, #stat_compare_means()
       corrplot, ## create correlation matrices easily
       cowplot, ## plot_grid() to put put multiple plots into one plot
       ggbiplot) ## simple package to make PCAs

## set a better graph theme
theme_set(theme_bw())

## Set common columns
common_cols <- c("kit_id", "transect_location")

# 2. Import data ---------------------------------------------------------------

## Load in useful datasets
bulk_density <- read_csv("data/test/multivariate_example/EC1_Soil_BulkDensity_L0B_20220714.csv")
gwc <- read_csv("data/test/multivariate_example/EC1_Soil_GWC_L0B_20220714.csv")
loi <- read_csv("data/test/multivariate_example/EC1_Soil_LOI_L0B_20220714.csv")

df <- full_join(bulk_density %>% select(common_cols, bulk_density_g_cm3), 
                gwc %>% select(common_cols, gwc_perc), 
                by = c(common_cols)) %>% 
  full_join(loi %>% select(common_cols, loi_perc), 
            by = c(common_cols))


# 3. Individual orrelation plots -----------------------------------------------

## First, let's look at the correlation between two factors. Let's do GWC and LOI
## which have a pretty strong relationship. My first step is to plot them so 
## I can look and see visually if there is a correlation: 
ggplot(df, aes(gwc_perc, loi_perc)) + ## select data and variables
  geom_point() + ## add points
  geom_smooth(method = "lm") ## add best fit line

## Now, because we have different sample types, we can divide our plot above into
## 4 different plots to see if that better explains the relationship between
## gwc and loi: 
ggplot(df, aes(gwc_perc, loi_perc)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~transect_location) ## create 'facets' for each transect_location
## So, that's really helpful, because we see that there are different slopes 
## based on different transect_locations, where the first plot doesn't really
## show us that. 

## A different, more confusing way to look at this could be
## to plot all 4 on the same plot: 
ggplot(df, aes(gwc_perc, loi_perc, color = transect_location)) + 
  geom_point() + 
  geom_smooth(method = "lm") 
## This is actually kinda helpful in this case, because it clearly shows that
## as we move from sediment to upland, the slope generally increases, which is 
## a useful finding we can use to help interpret our data. In this case, GWC
## is analogous to the amount of water, and LOI is analogous to the amount of 
## organic carbon, so while higher saturation correlates to higher organic carbon,
## smaller increases in saturation in the upland lead to larger increases in 
## organic carbon. This is the kind of finding we are looking for: correlations
## that explain relationships between soil properties which help us understand 
## how the ecosystem works.


# 4. Correlation matrices ------------------------------------------------------

## Individual correlations are great to explore a relationship you already think
## is interesting, but would take a long time if you have many variables like we
## do, which is where correlation matrices excel. We can take as many continuous
## (won't work on factor or character columns) variables as we want and see how 
## they all correlate to each other. Since we saw that transect location is 
## useful, but transect_location is also a factor, we need to convert it to a 
## numeric value. Don't worry how this code works: 

df <- df %>% 
  mutate(location_numeric = case_when(transect_location == "Sediment" ~ 1, 
                                      transect_location == "Wetland" ~ 2, 
                                      transect_location == "Transition" ~ 3, 
                                      transect_location == "Upland" ~ 4))

## Now let's prepare our data for correlations
df_cor <- df %>% 
  select(where(is.numeric)) %>% ## only select numeric variables
  drop_na() ## correlations fail if there are missing values (NAs) so drop em

## Time to make our first correlation matrix!
cor(df_cor)
## Interpreting this: Each value is the correlation coefficient between two 
## variables. You can see for the first row in the first column, there's a 
## correlation of 1. That's because bulk_density perfectly correlates with itself.
## Values closer to 1 mean strong positive correlations (when one variable increases, 
## the other increases). Values closer to -1 mean stronger negative correlations 
## (when one variable increases, the other decreases).

## An easier way to visualize this is with corrplot()
cor(df_cor) %>% 
  corrplot()
## Interpretation: We can see that bulk density is negatively correlated to
## both GWC and LOI. We can also see that GWC and LOI are positively correlated,
## which we knew from the plots in the last section. We don't see strong
## correlations between transect location and any of the variables, except maybe
## for GWC. 

## Since we saw much stronger correlations between GWC and LOI when separating
## by transect_location, let's do the corrplot again, but just for wetland
## soils: 
df %>% 
  filter(transect_location == "Wetland") %>% 
  select(where(is.numeric)) %>%
  drop_na() %>% 
  cor() %>% 
  corrplot()
## Note that since there's only one level for location_numeric, we don't get
## correlations. But now, we see a strong relationships between LOI and GWC. 


# 5. Correlation stats ---------------------------------------------------------

## Everything above is visual in that we don't know the exact numbers for any of
## these relationships. But we can get those pretty painlessly in the following 
## way. First, let's make a linear model relating LOI and GWC
lm(loi_perc~gwc_perc, data = df)

## This gives us two coefficients, which are b and m from y = mx + b. But we really
## want to know how well it fits (R2): 
summary(lm(loi_perc~gwc_perc, data = df))
## Down at the bottom, we see Adjusted R-squared = 0.4578, so R2 = 0.46. That's 
## decent, though an R2 > 0.6 would be more notable. One other important piece of
## info: the p-value down at the bottom is very very very low. Lower p-values mean
## a more significant R2, so that's good news.

## Now, since we know that we have stronger relationships when separating by 
## transect_location, let's do that (tidyverse to the rescue!)
summary(lm(loi_perc~gwc_perc, data = df %>% filter(transect_location == "Wetland")))
## R2 = 0.85! Much stronger! That's a great correlation and something we are very
## confident can be interpreted. 


# 6. Statistics to compare groups ----------------------------------------------

## One last thing: we often want to know compare mean values between different
## groups within the dataset. We'll use some ggpubr magic: 

## First, we need to tell ggplot what groups we want to compare
## Set comparisons for stats
compare_transect <- list( c("Sediment", "Wetland"), c("Sediment", "Transition"), 
                          c("Sediment", "Upland"), c("Wetland", "Transition"), 
                          c("Wetland", "Upland"), c("Transition", "Upland"))

## make the plot
ggplot(df, aes(transect_location, gwc_perc)) + 
  geom_boxplot() + ## add boxplots
  stat_compare_means(aes(label = ..p.signif..), comparisons = compare_transect)
## Here, ns means not significant, or a p-value >= 0.05. The stars mean that
## there is a significant difference (p<0.05) between the groups.





