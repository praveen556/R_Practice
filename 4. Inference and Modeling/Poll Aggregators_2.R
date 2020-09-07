library(tidyverse)
library(dslabs)

#Predicting Obama Poll 
d<-0.038
Ns<-c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2  #Probability of Dems winning

confidence_interval <- sapply(Ns, function(N){
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat *(1-X_hat)/N)
  2*c(X_hat, X_hat - 2 * SE_hat, X_hat + 2 * SE_hat )-1})

polls <- data.frame(poll=1:ncol(confidence_interval),t(confidence_interval), samp_size=Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

#Calculating the spread of combined polls

d_hat <- polls %>% summarise( avg=sum(estimate*sample_size)/sum(sample_size))%>% .$avg

p_hat <- (1+d_hat)/2

moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))
round(d_hat*100,1)
round(moe*100,1)