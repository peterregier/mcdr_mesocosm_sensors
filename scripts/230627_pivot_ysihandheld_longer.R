## Goal: pivot_longer so depth is a column, but the trick is do it repeatedly
## for each variable

require(pacman)
p_load(tidyverse)

df_raw <- read_csv("data/ysidata.csv")

## Found this here: https://stackoverflow.com/questions/59253987/parallel-pivot-longer-of-two-sets-of-columns
## I don't know how to get what I want, so time to take the lazy way out
df_raw %>% 
  filter(!is.na(sample_id)) %>% 
  select(-sample_id) %>% 
  pivot_longer(-Date, 
               names_to = c("Var", ".value"), 
               names_sep = "_")


## Create two dataframes, one for surface, one for deep
df_surface <- df_raw %>% 
  filter(!is.na(sample_id)) %>% 
  select(-sample_id) %>%
  select(Date, density, contains("_surface")) %>% ## Select just surface columns + metadata
  rename_with(~str_remove(., '_surface')) %>% ## Remove _surface so columns will match
  mutate(depth = "surface")

df_deep <- df_raw %>% 
  filter(!is.na(sample_id)) %>% 
  select(-sample_id) %>%
  select(Date, density, contains("_deep")) %>%  
  rename_with(~str_remove(., '_deep')) %>% 
  mutate(depth = "deep")

df <- bind_rows(df_surface, df_deep)

## checking my work
ggplot(df, aes(pH, do, color = density)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  facet_wrap(~depth, nrow = 1)

ggplot(df, aes(density, do, fill = depth)) + 
  geom_boxplot()
