source("clean.R")

#LIFE EXPECTANCY GROWTH RATE OVER TIME BY CONTINENT
statNames <- names(select(flash,lifeExp:popThous))
continentList <- lapply(statNames,function(x){
  flash %>%
    spread_("continent",x) %>%
    group_by(year) %>%
    summarise_at(vars(Africa:Oceania),function(col) median(col,na.rm=T)) %>%
    select(-year) %>% #must remove year because we do not need its growth rate
    as.matrix %>% log %>% diff %>%#growth rate transformation only possible as matrix
    tbl_df %>% #mutate() only possible with tibble (or data frame)
    mutate(year = seq(1977,2007,5)) %>%
    melt("year",1:5,variable.name="continent",value.name=paste(x)) %>%
    tbl_df -> flash2
  if(which(statNames == x) == 1){flash2}#if in first loop, lapply returns three columns
  else{select_(flash2,x)}#if not in first loop, lapply returns newly calculated column
})
for(i in 2:4) continentList[[1]] <- bind_cols(continentList[[1]],continentList[i])#binds columns from list
continentGrowthRates <- continentList[[1]]

continentGrowthRates %>%
  gather(stat,growth_rate,lifeExp:popMill) %>%
  ggplot(aes(year,growth_rate,color=stat)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~continent) + 
  theme_minimal()

continentGrowthRates %>%
  filter(continent != "Oceania") %>%
  #gather(stat,growth_rate,lifeExp:popMill) %>%
  ggplot(aes(year,gdpPercap,color=continent)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  #facet_wrap(~continent) + 
  theme_minimal() + 
  xlab("Year") + 
  ylab("GDP per capita growth rate")

