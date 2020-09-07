library(dslabs)
data(gapminder)
gapminder %>% head()
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)

#Histogram for 1970 countries based on dollar_per_day

gapminder %>% filter(year==1970 & !is.na(gdp)) %>% ggplot(aes(dollars_per_day))+geom_histogram(binwidth = 1, col="black")

#Histogram for 1970 countries based on dollar_per_day with log2

gapminder %>% filter(year==1970 & !is.na(gdp)) %>% ggplot(aes(log2(dollars_per_day)))+
                                                            geom_histogram(binwidth = 1, col="black")
#Histogram with Log2 values and scale as normal 

gapminder %>% filter(year==1970 & !is.na(gdp)) %>% ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1, col="black")+
  scale_x_continuous(trans = "log2")