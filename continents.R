source("clean.R")

#LIFE EXPECTANCY GROWTH RATE OVER TIME BY CONTINENT
statNames <- names(select(flash,lifeExp:popThous))
continentList <- lapply(statNames,function(x){
  flash %>%
    spread_("continent",x) %>%
    group_by(year) %>%
    summarise_at(vars(Africa:Oceania),function(col) mean(col,na.rm=T)) %>%
    select(-year) %>% #must remove year because we do not need its growth rate
    as.matrix %>% log %>% diff %>%#growth rate transformation only possible as matrix
    tbl_df %>% #mutate() only possible with tibble (or data frame)
    mutate(year = seq(1977,2007,5)) %>%
    melt("year",1:5,variable.name="continent",value.name=paste(x)) %>%
    tbl_df -> flash2 #melt defaults to data frame, which is unfortunate
  if(which(statNames == x) != 1){select_(flash2,x)}else{flash2}
})
for(i in 2:4) continentList[[1]] <- bind_cols(continentList[[1]],continentList[i])
continentGrowthRates <- continentList[[1]]

continentGrowthRates %>%
  gather(stat,growth_rate,lifeExp:popThous) %>%
  ggplot(aes(year,growth_rate,color=stat)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~continent) + 
  theme_minimal()
