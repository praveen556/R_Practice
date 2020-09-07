library(dslabs)
data("gapminder")

#Immportality Rate
gapminder %>% filter(year==2015 & country %in% c("Sri Lanka","Turkey")) %>% select(country, infant_mortality)
gapminder %>% filter(year==2015 & country %in% c("Poland","South Korea")) %>% select(country, infant_mortality)
gapminder %>% filter(year==2015 & country %in% c("Malaysia","Russia")) %>% select(country, infant_mortality)
gapminder %>% filter(year==2015 & country %in% c("Pakistan","Vietnam")) %>% select(country, infant_mortality)
gapminder %>% filter(year==2015 & country %in% c("Thailand","South Africa")) %>% select(country, infant_mortality)

#Life Expectancy  and Fertility Rate

ds_theme_set()
filter(gapminder, year==1962) %>%
  ggplot(aes(fertility, life_expectancy))+geom_point()

#Adding Color to the graph

filter(gapminder, year ==2015) %>%
  ggplot(aes(fertility, life_expectancy, color=continent))+
  geom_point()

#Facet function to show multiple graphs and sync the scale and facet by only year

gapminder %>% filter(year %in% c(1962, 2012)) %>% ggplot(aes(fertility,life_expectancy, col=continent))+
  geom_point()+
  facet_grid(~year)

#Facet function to show multiple graphs and sync the scale and facet by  year and continent

gapminder %>% filter(year %in% c(1962,2012)) %>% ggplot(aes(fertility, life_expectancy, col=continent))+
  geom_point()+
  facet_grid(continent~year)


# facet by year, plots wrapped onto multiple rows
years <- c(1962,1970,1980,1990,2012)
continents <- c("Asia","Europe")

gapminder %>% filter(year %in% years & continent %in% continents)%>%
  ggplot(aes(fertility, life_expectancy, col=continent))+
  geom_point()+
  facet_wrap(~year)


#facet wrap by year and countries

countries <- c("India","China","United Kingdom","United States")

gapminder %>% filter(year %in% years, country %in% countries) %>% 
  ggplot(aes(fertility, life_expectancy, col=country))+
  geom_point()+
  facet_wrap(~year)


# Timeplot Charts

gapminder %>% filter(country=="United States") %>% ggplot(aes(year,fertility))+
  geom_line()


# Timeplot Charts with multiple countries
countries <- c("United States", "India","China")

gapminder %>% filter(country %in% countries) %>% ggplot(aes(year,fertility, col=country))+
  geom_line()

#Adding labels

labels <- data.frame(country = countries, x=c(1970, 1980, 1980), y=c(2, 5, 3))

gapminder %>% filter(country %in% countries) %>% ggplot(aes(year,fertility, col=country))+
  geom_line()+
  geom_text(data=labels, aes(x,y, label=country), size=5)+
  theme(legend.position = "none")
