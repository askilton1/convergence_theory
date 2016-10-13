source("clean.r")
flash %>%
  group_by(continent_world,year) %>%
  summarise(median_gdpPerCap = median(gdpPercap)) %>%
  ggplot(aes(year,median_gdpPerCap,color=continent_world)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  theme_minimal() + 
  xlab("Year") + 
  ylab("Median GDP per capita")
