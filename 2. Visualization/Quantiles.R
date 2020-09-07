#To divide dataset into 100 equal parts
p <- seq(0.01,0.99,.02)
percentiles <- quantile(heights$height,p)
#To divide dataset into 4 parts
summary(heights$height)

#to check names of the 25th and 75th perncentile.
percentiles[names(percentiles)=="25%"]
percentiles[names(percentiles)=="75%"]

p
qnorm(p,69,3)

#Practise with tidyverse dataset

library(tidyverse)
library(dslabs)
data(heights)

index <- heights$sex == "Male"

x <- heights$height[index]
x
z <- scale(x)
z
#Proportion of data below 69.5
mean(x<=69.5)

#calculate observed and theretical quantile
p <- seq(0.01,0.99,0.01)
observed_quantiles <- quantile(x,p)
theoritical_quantile <- qnorm(p,mean = mean(x), sd = sd(x))

#make QQ Plot
plot(theoritical_quantile,observed_quantiles)
abline(0,1)

# make QQ-plot with scaled values
observed_quantiles <- quantile(z,p)
theoritical_quantile <- qnorm(p)
plot(theoritical_quantile,observed_quantiles)
abline(0,1)

