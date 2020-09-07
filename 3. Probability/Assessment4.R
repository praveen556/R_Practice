library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

p <- death_prob %>% filter(age==50 & sex=="Female") %>% pull(prob)

a <- -150000
b <- 1150

#What is the expected value of the company's net profit on one policy for a 50 year old female?

mu <- a*p + b*(1-p)
mu
#Calculate the standard error of the profit on one policy for a 50 year old female.

sigma <- abs(b-a) * sqrt(p*(1-p))
sigma
n<-1000
#What is the expected value of the company's profit over all 1,000 policies for 50 year old females?
n*mu

#What is the standard error of the sum of the expected value over all 1,000 policies for 50 year old females?
se <- sigma*sqrt(n)
se

#Use the Central Limit Theorem to calculate the probability that the insurance company loses money on this set of 1,000 policies.
pnorm(0, n*mu, se)

p_male <- death_prob %>% filter(age==50 & sex=="Male") %>% pull(prob)
p_male
p
#What premium should be charged?

p <- p_male
mu_sum <- 700000
n <- 1000
a <- -150000

b <- (mu_sum/n-a*p)/(1-p)
b
#Using the new 50 year old male premium rate, calculate the standard error of the sum of 1,000 premiums.
sigma_sum <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))
sigma_sum


#What is the probability of losing money on a series of 1,000 policies to 50 year old males?
pnorm(0, mu_sum, sigma_sum)

#In this 6-part question, we'll look at a scenario in which a lethal pandemic disease increases the probability of death within 1 year for a 50 year old to .015. Unable to predict the outbreak, the company has sold 1,000 $150,000 life insurance policies for $1,150.
#What is the expected value of the company's profits over 1,000 policies?
p <- .015    # probability of claim
a <- -150000    # loss per claim
b <- 1150    # premium - profit when no claim
n <- 1000

exp_val <- n*(a*p + b*(1-p))
exp_val

#What is the standard error of the expected value of the company's profits over 1,000 policies?

se <- sqrt(n)*abs(b-a) * sqrt(p*(1-p))
se

#What is the probability of company losing mony?
pnorm(0, exp_val, se)

#What is the probability of losing more than $1 million?
pnorm(-1*10^6, exp_val, se)

#Investigate death probabilities p <- seq(.01, .03, .001).
#What is the lowest death probability for which the chance of losing money exceeds 90%?
p <- seq(.01, .03, .001)
a <- -150000    # loss per claim
b <- 1150    # premium - profit when no claim
n <- 1000

p_lose_money <- sapply(p, function(p){
  exp_val <- n*(a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(0, exp_val, se)
})

data.frame(p, p_lose_money) %>%
  filter(p_lose_money > 0.9) %>%
  pull(p) %>%
  min()


#Investigate death probabilities p <- seq(.01, .03, .0025).
#What is the lowest death probability for which the chance of losing over $1 million exceeds 90%?
p <- seq(.01, .03, .0025)
a <- -150000    # loss per claim
b <- 1150    # premium - profit when no claim
n <- 1000
p_lose_million <- sapply(p, function(p){
  exp_val <- n*(a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(-1*10^6, exp_val, se)
})

data.frame(p, p_lose_million) %>%
  filter(p_lose_million > 0.9) %>%
  pull(p) %>%
  min()

#Define a sampling model for simulating the total profit over 1,000 loans with probability of claim p_loss = .015, loss of -$150,000 on a claim, and profit of $1,150 when there is no claim. Set the seed to 25, then run the model once.
#What is the reported profit (or loss) in millions (that is, divided by  106 )?


set.seed(25)

p <- .015
loss <- -150000
profit <- 1150
n <- 1000

outcomes <- sample(c(loss, profit), n, prob = c(p, 1-p), replace = TRUE)
sum(outcomes)/10^6

#Set the seed to 27, then run a Monte Carlo simulation of your sampling model with 10,000 replicates to simulate the range of profits/losses over 1,000 loans.
#What is the observed probability of losing $1 million or more?

set.seed(27)
B <- 10000

profits <- replicate(B, {
  outcomes <- sample(c(loss, profit), n, prob = c(p, 1-p), replace = TRUE)
  sum(outcomes)/10^6
})

mean(profits < -1)

#Suppose that there is a massive demand for life insurance due to the pandemic, and the company wants to find a premium cost for which the probability of losing money is under 5%, assuming the death rate stays stable at  p=0.015 .
p <- .015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

mu <- l*p + x*(1-p)
mu

#What is the expected profit over 1,000 policies?
b <- mu*n  

#Run a Monte Carlo simulation with B=10000to determine the probability of losing money on 1,000 policies given the new premium x, loss on a claim of $150,000, and probability of claim  p=.015 . Set the seed to 28 before running your simulation.

#What is the probability of losing money here?
set.seed(28)
B <- 10000
profit <- replicate(B, {
  draws <- sample(c(x, l), n,
                  prob=c(1-p, p), replace = TRUE)
  sum(draws)
})

mean(profit < 0)

#The company cannot predict whether the pandemic death rate will stay stable. Set the seed to 29, then write a Monte Carlo simulation that for each of  B=10000  iterations:

set.seed(29)
l <- -150000
n <- 1000
B <- 10000
x <- 3268

profit <- replicate(B, {
  new_p <- 0.015+sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c( l,x), n, 
                   prob=c( new_p,1-new_p), replace = TRUE)
  sum(draws)
})

mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -1000000)    # probability of losing over $1 million
