library(tidyverse)
library(dslabs)
data(gapminder)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

#Slop Chart
dat <- gapminder %>% filter(year %in% c(2010,2015) & region %in% west & !is.na(life_expectancy) & population> 10^7 )

dat %>% mutate(location=ifelse(year==2010,1,2),
               location=ifelse(year==2015& country %in% c("United Kingdom","Portugal"),location+0.22,location),
               hjust=ifelse(year==2010,1,0),
               year = as.factor(year))%>%
  ggplot(aes(year,life_expectancy, group=country))+
  geom_line(aes(color=country), show.legend = FALSE)+
  geom_text(aes(x=location, label=country, hjust=hjust ), show.legend = FALSE)+
              xlab("")+
              ylab("Life Expectancy")

dat

#Bland-Altman Plot

library(ggrepel)

dat %>% mutate(year = paste0("Life_Expectanc5y_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year,life_expectancy)%>%
  mutate(average=(Life_Expectancy_2010+Life_Expectancy_2015)/2, difference = Life_Expectancy_2015-Life_Expectancy_2010)%>%
  ggplot(aes(average,difference, label=country))+
  geom_point()+
  geom_text_repel()+
  geom_abline(lty=2)+
  xlab("Average of 2010 and 2015")+
  ylab("Difference Between 2015 and 2010")