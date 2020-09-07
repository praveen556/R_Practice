#What is the probability of guessing correctly for one question?
p<-1/5
p

#What is the expected value of points for guessing on one question?
a <-1
b <--0.25

mu <- (a*p)+(b*(1-p))
mu

#What is the expected score of guessing on all 44 questions?
n <- 44

n*mu

#What is the standard error of guessing on all 44 questions?

sigma <- sqrt(n) * abs(b-a) *sqrt(p*(1-p))
sigma

#Use the Central Limit Theorem to determine the probability that a guessing student scores 8 points or higher on the test.

1-pnorm(8,mu,sigma)

#Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test.

set.seed(21)
RNGkind(sample.kind = "Rounding")



#What is the probability that a guessing student scores 8 points or higher?
set.seed(21, sample.kind = "Rounding")
B <- 10000
n <- 44
p <- 0.2
tests <- replicate(B, {
  X <- sample(c(1, -0.25), n, replace = TRUE, prob = c(p, 1-p))
  sum(X)
})
mean(tests >= 8)

#The SAT was recently changed to reduce the number of multiple choice options from 5 to 4 and also to eliminate the penalty for guessing.

#What is the expected value of the score when guessing on this new test?

p<-1/4
p
a <-1
b <-0

mu <- (a*p)+(b*(1-p))
mu

n <- 44

n*mu

#Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) representing a range of student skills.

#What is the lowest p such that the probability of scoring over 35 exceeds 80%?

p <- seq(0.25, 0.95, 0.05)

exp_val <- sapply(p, function(x){
  mu <- n* (a*x)+(b*(1-x))
  sigma <- sqrt(n)*abs(b-a)*sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})
min(p[which(exp_val>0.8)])
  
