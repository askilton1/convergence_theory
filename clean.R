#library(devtools);install_github("askilton1/antonioSkiltonTools")
library(antonioSkiltonTools);library(dplyr);library(tidyr);library(ggplot2);library(stringr);library(reshape2)

read.csv("4_Flash_Proj_1_Data.csv") %>% tbl_df %>%
  mutate(country = str_replace_all(country,"[.,]",""),
         country = as.factor(country),
         gdpPercap = gdpPercap/1000,
         popMill = popThous/1000) %>%
  mutate(first_world = ifelse(continent == "Europe" | continent == "Oceania" | country == "United States",1,0),
         continent_world = ifelse(first_world == 1, "First World", as.character(continent))) %>%
  select(-popThous) -> flash
