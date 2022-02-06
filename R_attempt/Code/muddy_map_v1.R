library(tidyverse)
here::i_am("Code/muddy_map_v1.R")
library(here)
library(ceylon)

library(magrittr)



df_raw <- readxl::read_xlsx(here("Data/Electoral_Votes.xlsx"), col_names = F)
colnames(df_raw) <- c("District", "Province", "GR", "GR_percent", "SP", "SP_percent", "AKD", "AKD_percent", "popuplation")

df_raw %>% head()

# district
data(district)

ggplot(district) + 
  geom_sf(aes(fill = DISTRICT))

df_raw %<>% 
  mutate(
    DISTRICT = toupper(District)
  ) 

district %>% 
  inner_join(df_raw, by = "DISTRICT") -> df


ggplot(df) + 
  geom_sf(aes(fill = GR_percent), show.legend = T) + 
  scale_fill_viridis_c(option = "B")

# resources
# 1. Muddy Map US election
# 2. Use of bivariate color scale - reddit: https://www.reddit.com/r/dataisbeautiful/comments/s1axlz/oc_2_years_into_the_pandemic_a_map_of_the/hs71380/
# 3. User of bivariate color paletters in R: https://www.r-bloggers.com/2020/08/how-to-choose-a-bivariate-color-palette/


# Find more resouces

## Bivariate Color palettes

library(colorblindcheck)
library(pals)



# Create Color palette

# 1. Variable One --> GR Percentage
# 2. Variable Two --> Population



# GR Percent
# Since the percentage of votes is highly variant, we'll use the base
# bins of 0-> 0.2, 0.2 -> 0.4 .... 0.8 -> 1.00

GR_bins <- seq(0, 1, 0.01) %>% quantile(probs = seq(0, 1, length.out = 4))


# Population
# Since population has an upper and lower limit, we'll use them abs bins
# to create base bins

pop_bins <- df %>% pull(population) %>% quantile(probs = seq(0, 1, length.out = 4))


tibble(
  "3-3" = "#d60066", # High GR High Pop
  "2-3" = "#0a50a1", 
  "1-3" = "#008837", # Low GR High Pop
  
  "3-2" = "#f668b3", # High GR medium pop
  "2-2" = "#85a8d0",
  "1-2" = "#80c39b", # Log GR Medium pop
  
  "3-1" = "#fbb4d9", # high gr low pop
  "2-1" = "#cedced",
  "1-1" = "#cce8d7"  # low gr low pop
) %>% 
  gather("group", "fill") -> color_scale

# Put data into bins

df %>% 
  mutate(
    gr_quantile = cut(
      GR_percent, 
      breaks = GR_bins, 
      include.lowest = FALSE
    ), 
    
    pop_quantile = cut(
      population, 
      breaks = pop_bins, 
      include.lowest = TRUE
    ), 
    
    group = paste0(as.numeric(gr_quantile), '-', as.numeric(pop_quantile))
  ) %>% 
  
  left_join(color_scale, by = "group") -> df_scaled


ggplot(data = df_scaled) + 
  scale_alpha(name = "", range = c(0.6, 0), guide = FALSE) + 
  geom_sf(aes(fill = fill), color = "white", size = 0.1) + 
  scale_fill_identity()


# check scales --> check gr - pop
# insert raster --> from (HM)
# check district
# Resources = https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/


# check municipal regions
