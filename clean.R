#library(devtools);install_github("askilton1/antonioSkiltonTools")
library(antonioSkiltonTools);library(dplyr);library(tidyr);library(ggplot2);library(stringr);library(reshape2)
flash <- tbl_df(read.csv("4_Flash_Proj_1_Data.csv"))

flash %>%
  mutate(country = str_replace_all(country,"[.,]",""),
         country = as.factor(country),
         gdpPercap = gdpPercap/1000,
         popMill = popThous/1000) %>% 
  select(-popThous) -> flash
