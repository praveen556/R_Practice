# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

#Brexit poll analysis - Part 1

#Question 1: Expected value and standard error of a poll
#The final proportion of voters choosing "Remain" was ????=0.481. Consider a poll with a sample of ????=1500 voters.

#What is the expected total number of voters in the sample choosing "Remain"?
p <- 0.481
N <- 1500
N*p

#What is the standard error of the total number of voters in the sample choosing "Remain"?
sqrt(N*p*(1-p))

#What is the expected value of ????^ , the proportion of "Remain" voters?
p=0.481

#What is the standard error of ????^ , the proportion of "Remain" voters?
se_hat<-sqrt(p*(1-p)/N)
se_hat

#What is the expected value of ????, the spread between the proportion of "Remain" voters and "Leave" voters?
2*p-1

#What is the standard error of ????, the spread between the proportion of "Remain" voters and "Leave" voters?
2*sqrt(p*(1-p)/N)

#Question 2: Actual Brexit poll estimates
#Load and inspect the brexit_polls dataset from dslabs, which contains actual polling data for the 6 months before the Brexit vote.
#Calculate x_hat for each poll, the estimate of the proportion of voters choosing "Remain" on the referendum day (????=0.481), 
#given the observed spread and the relationship ????^ =2????^ ???1.
#What is the average of the observed spreads (spread)?

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)
mean(brexit_polls$spread)

#What is the standard deviation of the observed spreads?
sd(brexit_polls$spread)

#What is the average of x_hat, the estimates of the parameter ?????
mean(brexit_polls$x_hat)

#What is the standard deviation of x_hat?
sd(brexit_polls$x_hat)

#Question 3: Confidence interval of a Brexit poll
#Consider the first poll in brexit_polls, a YouGov poll run on the same day as the Brexit referendum:
yougov <- brexit_polls[1,]

#Use qnorm() to compute the 95% confidence interval for ????^.

#X_hat - qnohe lower and upper bounds of the 95% confidence interval?
se  <- sqrt(yougov$x_hat *(1-yougov$x_hat)/yougov$samplesize)
bounds <- c(lower_bound=yougov$x_hat - qnorm(0.975)*se, upper_bound=yougov$x_hat + qnorm(0.975)*se)

#Does the bounds95% confidence interval predict a winner (does not cover ????=0.5)? 
#Does the 95% confidence interval cover the true value of ???? observed during the referendum (????=0.481)?
!between(0.5, bounds[1], bounds[2])    # predicts winner
between(0.481, bounds[1], bounds[2])   # does not cover p