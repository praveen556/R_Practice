#The death_prob data frame from the dslabs package contains information about the estimated probability of death within 1 year (prob) for different ages and sexes.
library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

death_prob

#Use death_prob to determine the death probability of a 50 year old female, p.
p <- death_prob %>%
  filter(sex == "Female" & age == "50") %>%
  pull(prob)
p

#The loss in the event of the policy holder's death is -$150,000 and the gain if the policy holder remains alive is the premium $1,150.
#What is the expected value of the company's net profit on one policy for a 50 year old female?
a <- -150000
b <- 1150

mu<-b*(1-p)+(a*p)
mu

#Calculate the standard error of the profit on one policy for a 50 year old female.
sigma <- abs(b-a) * sqrt(p*(1-p))
sigma

#What is the expected value of the company's profit over all 1,000 policies for 50 year old females?
n <-1000
n*mu

#What is the standard error of the sum of the expected value over all 1,000 policies for 50 year old females?

sqrt(n)*sigma


#Use the Central Limit Theorem to calculate the probability that the insurance company loses money on this set of 1,000 policies.

pnorm(0,n*mu,sqrt(n)*sigma)


#50 year old males have a different probability of death than 50 year old females. We will calculate a profitable premium for 50 year old males in the following four-part question.

#Use death_prob to determine the probability of death within one year for a 50 year old male.

p_male<-death_prob%>%filter(sex=="Male" & age==50)%>%pull(prob)
p_male

#Suppose the company wants its expected profits from 1,000 50 year old males with $150,000 life insurance policies to be $700,000. Use the formula for expected value of the sum of draws with the following values and solve for the premium  

p <- p_male
mu_sum <- 700000
n <- 1000
a <- -150000

b <- (mu_sum/n-a*p)/(1-p)
b


#Using the new 50 year old male premium rate, calculate the standard error of the sum of 1,000 premiums.

sigma_male <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))
sigma_male

#What is the probability of losing money on a series of 1,000 policies to 50 year old males?
pnorm(0, mu_sum, sigma_male)

#What is the expected value of the company's profits over 1,000 policies?

p <- .015    # probability of claim
a <- -150000    # loss per claim
b <- 1150    # premium - profit when no claim
n <- 1000

exp_val <- n*(a*p + b*(1-p))
exp_val

#What is the standard error of the expected value of the company's profits over 1,000 policies?

se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
se

#What is the probability of the company losing money?
pnorm(0,exp_val,se)

#Suppose the company can afford to sustain one-time losses of $1 million, but larger losses will force it to go out of business.

#What is the probability of losing more than $1 million?

pnorm(-1*10^6,exp_val,se)

#Investigate death probabilities p <- seq(.01, .03, .001)
#What is the lowest death probability for which the chance of losing money exceeds 90%?

p <- seq(.01, .03, .001)
a <- -150000    # loss per claim
b <- 1150    # premium - profit when no claim
n <- 1000


p_lose_money <- sapply(p, function(p){
  exp_val<-n*((a*p)+(b*(1-p)))
  std_error <- sqrt(n) *abs(b-a)*sqrt(p*(1-p))
  pnorm(0,exp_val, std_error)
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


p_lose_money <- sapply(p, function(p){
  exp_val<-n*((a*p)+(b*(1-p)))
  std_error <- sqrt(n) *abs(b-a)*sqrt(p*(1-p))
  pnorm(-1*10^6,exp_val, std_error)
})
data.frame(p, p_lose_money) %>%
  filter(p_lose_money > 0.9) %>%
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

#Calculate the premium required for a 5% chance of losing money given  n=1000  loans, probability of death  p=0.015 , and loss per claim  l=???150000 . Save this premium as x for use in further questions.

p <- .015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

#What is the expected profit per policy at this rate?

l*p + x*(1-p)


#What is the expected profit over 1,000 policies?

mu <- n*(l*p + x*(1-p))
mu

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


#What is the expected value over 1,000 policies?


# n, p, l and x as defined in the problem information
set.seed(29)    # in R 3.6, set.seed(29, sample.kind="Rounding")

profit <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample(c(x, l), n, 
                  prob=c(1-new_p, new_p), replace = TRUE) 
  sum(draws)
})


#What is the probability of losing money?

mean(profit < 0)

#What is the probability of losing more than $1 million?
mean(profit < -1*10^6)
