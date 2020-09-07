library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(heights$sex=="Male") %>% pull(height)
x
#probability of Male taller than 70.5 inch

1-pnorm(70.5,mean(x),sd(x))

#Discretization and the normal approximation
#plot distribution of exact heights in the data
plot(prop.table(table(x)),xlab = "a = Height in Inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x<=68.5) - mean(x<=67.5)
mean(x<=69.5) - mean(x<=68.5)
mean(x<=70.5) - mean(x<=69.5)


#Getting the data using pnorm

pnorm(68.5,mean(x),sd(x)) - pnorm(67.5,mean(x),sd(x))
pnorm(69.5,mean(x),sd(x)) - pnorm(68.5,mean(x),sd(x))
pnorm(70.5,mean(x),sd(x)) - pnorm(69.5,mean(x),sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


