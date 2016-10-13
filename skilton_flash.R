libraries <- c("dplyr","tidyr","ggplot2","stringr","reshape2","gridExtra","knitr","stargazer")
lapply(libraries,require,character.only = TRUE)

read.csv("4_Flash_Proj_1_Data.csv") %>% tbl_df %>%
  mutate(country = str_replace_all(country,"[.,]",""),
         country = as.factor(country),
         gdpPercap = gdpPercap/1000,
         popMill = popThous/1000) %>%
  mutate(first_world = ifelse(continent == "Europe" | 
                                continent == "Oceania" | 
                                country == "United States", 1, 0),
         continent_world = ifelse(first_world == 1, 
                                  "First World", 
                                  as.character(continent))) %>%
  select(-popThous) -> flash

#Life Expectancy

flash %>%
  group_by(country,continent) %>%
  summarise_at(vars(lifeExp:popMill),mean) %>%
  lm(lifeExp ~ poly(gdpPercap,3) + continent,.) -> mod


flash %>%
  select(continent_world,country,lifeExp,gdpPercap,popMill) %>%
  group_by(country,continent_world) %>%
  summarise_if(is.numeric,mean) %>%
  ungroup %>%
  mutate(predicted = predict(mod),
         difference = lifeExp-predicted,
         label = ifelse(abs(difference) > 9,as.character(country),NA)) -> flashForPlot

flashForPlot %>%
  filter(country != "Australia") %>%
  ggplot() + 
  geom_point(aes(gdpPercap, lifeExp,color=label)) + 
  geom_point(aes(gdpPercap, predicted)) +
  geom_segment(aes(x = gdpPercap, y = lifeExp,
                   xend = gdpPercap, yend = predicted,
                   color = label)) +
  geom_smooth(aes(gdpPercap, predicted),se = FALSE) +
  facet_wrap( ~ continent_world, scales = "free_x") +
  theme_minimal() + 
  theme(legend.position = "none",
        panel.grid.major.y = element_blank()) +
  xlab("GDP per capita") + 
  ylab("Life expectancy") + 
  ggtitle("Life Expectancy by GDP per capita")

#Convergence
statNames <- names(select(flash,lifeExp:popMill))

continentList <- lapply(statNames,function(x){
  flash %>%
    spread_("continent",x) %>%
    group_by(year) %>%
    summarise_at(vars(Africa:Oceania),function(col) median(col,na.rm=T)) %>%
    select(-year) %>% #must remove year because we do not need its growth rate
    as.matrix %>% #growth rate transformation only possible as matrix
    log %>% diff %>% #calculate growth rates as the difference of the log values
    tbl_df %>% #mutate() only possible with tibble (or data frame)
    mutate(year = seq(1977,2007,5)) %>%
    melt("year",1:5,variable.name="continent",value.name=paste(x)) %>%
    tbl_df -> flash2
  if(which(statNames == x) == 1){flash2}#if in first loop, lapply returns three columns
  else{select_(flash2,x)}#if not in first loop, lapply returns newly calculated column
})


for(i in 2:4) continentList[[1]] <- bind_cols(continentList[[1]],continentList[i])

continentGrowthRates <- continentList[[1]]

continentGrowthRates %>%
  filter(continent != "Oceania") %>%
  ggplot(aes(year,gdpPercap,color=continent)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  theme_minimal() + 
  xlab("Year") + 
  ylab("Median GDP per capita growth rate") + 
  ggtitle("Median GDP per capita growth rate by Year") +
  theme(legend.position = "bottom",
        legend.title = element_blank())

#Median GDP Per Capita

flash %>%
  group_by(continent_world,year) %>%
  summarise(median_gdpPerCap = median(gdpPercap)) %>%
  ggplot(aes(year,median_gdpPerCap,color=continent_world)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  theme_minimal() + 
  xlab("Year") + ylab("Median GDP per capita") + ggtitle("Median GDP per capita by Year") +
  theme(legend.position = "bottom",
        legend.title = element_blank())

