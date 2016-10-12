source("clean.R")
library(knitr)

statNames <- select(flash,lifeExp:popMill) %>% names

lapply(statNames,function(x){
  flash %>%
    group_by(continent,year) %>%
    select_("continent","year",x) %>%
    summarise_if(is.numeric,mean) %>%
    spread_("continent",x) %>%
    round(1) %>%
    kable(.,caption=x)
})#rows = years, columns = continents, each table is one stat

lapply(statNames,function(x){
  flash %>%
    filter(continent == "Africa") %>%
    group_by(country,year) %>%
    select_("country","year",x) %>%
    summarise_if(is.numeric,mean) %>%
    spread_("country",x) %>%
    round(1) -> temp
  temp[,c(1,order(sapply(temp[,-1],mean),decreasing=T) + 1)] #%>%
    #kable(.,caption = x)
})##rows = years, columns = countries, each table is one stat, all tables are Africa

lapply(statNames,function(x){
  flash %>%
    filter(continent == "Americas") %>%
    group_by(country,year) %>%
    select_("country","year",x) %>%
    summarise_if(is.numeric,mean) %>%
    spread_("country",x) %>%
    round(1) -> temp
  temp[,c(1,order(sapply(temp[,-1],mean),decreasing=T) + 1)] #%>%
  #kable(.,caption = x)
})##rows = years, columns = countries, each table is one stat, all tables are Americas

lapply(statNames,function(x){
  flash %>%
    filter(continent == "Asia") %>%
    group_by(country,year) %>%
    select_("country","year",x) %>%
    summarise_if(is.numeric,mean) %>%
    spread_("country",x) %>%
    round(1) -> temp
  temp[,c(1,order(sapply(temp[,-1],mean),decreasing=T) + 1)] #%>%
  #kable(.,caption = x)
})##rows = years, columns = countries, each table is one stat, all tables are Americas

lapply(statNames,function(x){
  flash %>%
    filter(continent == "Europe") %>%
    group_by(country,year) %>%
    select_("country","year",x) %>%
    summarise_if(is.numeric,mean) %>%
    spread_("country",x) %>%
    round(1) -> temp
  temp[,c(1,order(sapply(temp[,-1],mean),decreasing=T) + 1)] #%>%
  #kable(.,caption = x)
})##rows = years, columns = countries, each table is one stat, all tables are Americas

flash %>%
  group_by(continent,year) %>%
  select(continent,year,lifeExp) %>%
  summarise_if(is.numeric,mean) %>%
  spread(continent,lifeExp)

flash %>%
  group_by(country,year) %>%
  select(continent,year,lifeExp) %>%
  summarise_if(is.numeric,mean) %>%
  spread(country,lifeExp)