source("clean.R")

#LIFE EXPECTANCY GROWTH RATE OVER TIME BY country
statNames <- names(select(flash,lifeExp:popThous))
countryList <- lapply(statNames,function(x){
  flash %>%
    spread_("country",x) %>%
    group_by(year) %>%
    summarise_at(vars(Algeria:Zimbabwe),function(col) mean(col,na.rm=T)) %>%
    select(-year) %>% #must remove year because we do not need its growth rate
    as.matrix %>% log %>% diff %>%#growth rate transformation only possible as matrix
    tbl_df %>% #mutate() only possible with tibble (or data frame)
    mutate(year = seq(1977,2007,5)) %>%
    melt("year",1:71,variable.name="country",value.name=paste(x)) %>%
    left_join(.,select(flash,country,continent),by="country") %>% #return continents
    tbl_df -> flash2 #melt defaults to data frame, which is unfortunate
  if(which(statNames == x) != 1){select_(flash2,x)}else{flash2}
})
for(i in 2:length(statNames)) countryList[[1]] <- bind_cols(countryList[[1]],countryList[i])
flashCountryGrowthRates <- countryList[[1]]

continentGrowthRates_fun <- function(tibble,continent_name){
  tibble %>%
    gather(stat,growth_rate,lifeExp:popThous) %>%
    filter(continent == "Africa") %>%
    ggplot(aes(year,growth_rate,color=country)) +
    geom_line() +
    geom_hline(yintercept = 0) + 
    facet_grid(stat~continent,scales="free") + 
    theme_minimal() + 
    theme(legend.position="none")
} 

# countryGrowthRates_fun("Africa")
# countryGrowthRates_fun("Americas")

countryGrowthRates_fun <- function(tibble, continent_name){
  tibble %>%
  gather(stat,growth_rate,lifeExp:popThous) %>%
  left_join(.,select(flash,country,continent),by="country") %>% 
  filter(continent == continent_name) %>%
  ggplot(aes(year,growth_rate,color=stat)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~country) + 
  theme_minimal()}

# countryGrowthRates_fun("Africa")
# countryGrowthRates_fun("Americas")

#DIMINISHING RETURNS OF GDP PER CAPITA
flash %>%
  group_by(country,continent) %>%
  summarise_at(vars(lifeExp:popMill),mean) %>%
  lm(lifeExp ~ poly(gdpPercap,3) + continent,.) -> mod
  
flash %>%
  select(continent,country,lifeExp,gdpPercap,popMill) %>%
  group_by(country,continent) %>%
  summarise_if(is.numeric,mean) %>%
  ungroup %>%
  mutate(predicted = predict(mod),
         difference = lifeExp-predicted,
         label = ifelse(abs(difference) > 9,as.character(country),NA)) -> flashForPlot

flashForPlot %>%
  filter(country != "Australia") %>%
ggplot() + 
  geom_point(aes(gdpPercap,lifeExp,color=label#,size=popMill
                 )) + 
  geom_point(aes(gdpPercap,predicted)) +
  geom_segment(aes(gdpPercap,lifeExp, xend = gdpPercap, yend = predicted, color = label)) +
  geom_smooth(aes(gdpPercap,predicted#,color=continent
                  ),se=FALSE) +
  theme_minimal() + #facet_grid(continent~.,scales = "free") +
  facet_wrap(~continent, scales = "free_x") +
  theme(legend.position = "none") -> plot1

flashForPlot %>%
  filter(country != "Australia") %>%
  mutate(country = factor(country,levels=country[order(abs(difference),decreasing = T)],ordered=T)) %>%
ggplot(aes(country,difference,fill=label,alpha=abs(difference))) + 
  geom_bar(stat="identity",position="dodge") +
  theme_minimal() + 
  theme(legend.position="top",
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_blank()) + #legends are for transparency and color
  facet_wrap(~continent, scales = "free_x") +
  guides(alpha=FALSE) -> plot2

library(gridExtra)
grid.arrange(plot1,plot2)
