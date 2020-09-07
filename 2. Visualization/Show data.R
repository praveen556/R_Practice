library(dslabs)
data(heights)

#Default chart with scatter Plot
heights %>% ggplot(aes(sex,height)) + geom_point()

#Adding alpha blending

heights %>% ggplot(aes(sex,height)) + geom_jitter(width = 0.1, alpha=0.2, colour="black", shape="diamond")

#Adding alpha blending + box plot

heights %>% ggplot(aes(sex,height))+geom_boxplot(fill =NA ) + geom_jitter(width = 0.1, alpha=0.2, colour="black", shape="diamond")


library(dplyr)
library(ggplot2)
library(dslabs)
dat <- us_contagious_diseases %>%
  filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>% reorder(rate)

state <- dat$state 
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)
state
levels(state)





library(dplyr)
library(ggplot2)
library(dslabs)
data(us_contagious_diseases)
dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>% arrange(rate)
dat
dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()
