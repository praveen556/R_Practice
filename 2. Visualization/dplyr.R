library(tidyverse)
library(dslabs)
data(heights)

#Compute average and standard deviation into a single table s
s <- heights %>% filter(sex=="Male") %>% summarise(average = mean(height), standard_deviation = sd(height))
s

heights %>% filter(sex=="Female") %>% summarise(average = mean(height), minimum = min(height),  maximum = max(height))

#Alternate way to get minmedian and max

quantile(heights$height,c(0, 0.5, 1))

#We cannot use quantile function inside summarise function. Summarise should be used only when we need one row
