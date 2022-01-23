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