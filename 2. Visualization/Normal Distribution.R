#Defining Vector for Male Heights
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]

#calculate Mean and Standard Deviation Manually

average <- sum(x)/length(x)
average
SD <- sqrt(sum((x - average)^2)/length(x))
SD

#Building with Mean and sd functions

average <- mean(x)
average
SD <- sd(x)
SD
c(average=average1, SD=SD1)

#Calculate standard units

z <- scale(x)

#Calculate Proportion of values with in 2 SD of mean
mean(abs(z)<2)
