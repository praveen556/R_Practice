#Roullette Wheel using Urn

color <- rep(c("Black","Red","Green"), times=c(18,18,2))

#Checking for 1000 independent draws
n <- 1000

X <- sample(ifelse(color=="Red",-1,1), n, replace = TRUE)
X[1:10]

#As we know probability of Red or non Red we can run it using below code

X <- sample(c(-1,1),n, replace = TRUE, prob = c(18/38,20/38))#1000 Independent draws

#Total Winning
S <- sum(X)
S

#Monto Carlo Simulation for the same 1000 Draws

B <- 10000

Winning <- replicate(B, {
  X <- sample(c(-1,1),n, replace = TRUE, prob = c(18/38,20/38))#1000 Independent draws
  S <- sum(X)
})
mean(Winning<0)#Probability of Casino loosing the money
mean(Winning)#Average Amount winning by Casino

#Histogram for the values

library(tidyverse)

s<- seq(min(Winning),max(Winning),length = 100)  # sequence of 100 values across range of S

normal_density <- data.frame(s=s,f=dnorm(s,mean(Winning),sd(Winning)))# generate normal density for S

data.frame(S=Winning) %>% # make data frame of S for histogram
  ggplot(aes(Winning, ..density..))+
  geom_histogram(color="black", binwidth=10)+
  ylab("Probability")+
  geom_line(data=normal_density, mapping = aes(s,f), color="blue")

