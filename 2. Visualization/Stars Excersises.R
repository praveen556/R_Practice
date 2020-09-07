library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3) 

stars
mean(stars$magnitude)
sd(stars$magnitude)

stars %>% ggplot(aes(magnitude))+geom_density()

stars %>% ggplot(aes(temp,y=..count..))+geom_density()

#scatterplot

stars %>% ggplot(aes(magnitude,temp,color=type))+geom_point(size=5)
