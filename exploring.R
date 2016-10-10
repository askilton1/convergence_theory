source("clean.R")

#life expectancy by year, all countries (total mess)
flash %>%
  ggplot(aes(x=year,y=lifeExp,color=country)) +
  geom_line()

#life expectancy by year, all continents
flash %>%
  group_by(continent,year) %>%
  summarise_if(is.numeric,mean) %>%
  ggplot(aes(x=year,y=lifeExp,color=continent)) +
  geom_line()

#gdpBillions, gdpPercap, lifeExp, and popThous by year (facets)
flash %>%
  gather(key,value,lifeExp:popThous) %>%
  group_by(continent,year,key) %>%
  summarise(value = mean(value)) %>%
  ggplot(aes(x=year,y=value,color=continent)) +
  geom_line() + facet_grid(key~.,scales="free")


flash %>%
  filter(continent == "Europe",
         year == 1977) %>%
  ggplot(aes(lifeExp,gdpPercap,color=country,size=gdpBillions)) +  
  geom_point() + 
  #facet_grid(~continent) +
  theme_minimal() + 
  theme(legend.position = "bottom")


