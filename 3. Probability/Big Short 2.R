#Interest rate sampling model, Bank income with 0% Interest Rate
n <- 1000
loss_per_foreclosure <- -200000
p<-0.02

defaults <- sample(c(0,1),n,prob = c(p,1-p), replace = TRUE)
sum(defaults*loss_per_foreclosure)

#Bank income with 0% Interest Rate Monto Carlo simulation

B<- 10000

losses <- replicate(B, {
  defaults <- sample(c(0,1),n,prob = c(p,1-p), replace = TRUE)
  sum(defaults*loss_per_foreclosure)
  })
mean(losses)

#Plotting expected losses based on Monto Carlo Simulation
library(tidyverse)
data.frame(losses_in_milloon=losses/10^6)%>%
  ggplot(aes(losses_in_milloon))+
  geom_histogram(color="black", binwidth=0.6)


#Expected value and standard error of the sum of 1,000 loans
mu<- n*((p*loss_per_foreclosure)+((1-p)*0))
mu
#Standard error
sigma <- sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))
sigma

#Calculating interest rates for expected value of 0

x <- -loss_per_foreclosure*p/(1-p)
x

#On the Loan of 180,000 Interest rate will be

x/180000


#Calculating interest rate for 1% probability of losing money

l <- loss_per_foreclosure
z <- qnorm(0.01)

x <- -l *(((n*p)-z*sqrt(n*p*(1-p)))/((n*(1-p))+z*sqrt(n*p*(1-p))))
x/180000# interest rate

loss_per_foreclosure*p+(x*(1-p)) # expected value of the profit per loan

n*(loss_per_foreclosure*p+(x*(1-p))) # Total Expected Profit for 1000 Loans

#Monte Carlo simulation for 1% probability of losing money

B<-10000

profit <- replicate(B,{
  draws <- sample(c(x,loss_per_foreclosure),n,prob = c(1-p,p), replace = TRUE )
  sum(draws)
})
mean(profit) # Profit for 1000 Loans
mean(profit<0) # Approximate loss Percentage of loans


#Expected value with higher default rate and interest rate

r <- 0.05
x <- r*180000
p<-0.04
loss_per_foreclosure<--200000
loss_per_foreclosure*p + x*(1-p)

#Calculating number of loans for desired probability of losing money

z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans


#Monte Carlo simulation with known default probability
B<-10000
p <-0.04
x <- 0.05*180000

profit <- replicate(B,{
  draws <- sample(c(x,loss_per_foreclosure),n, prob = c(1-p,p), replace = TRUE)
  sum(draws)
})
mean(profit)


#Monte Carlo simulation with unknown default probability

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01,0.01, length=100), 1)
  draws <- sample(c(x,loss_per_foreclosure),n, prob = c(1-new_p,new_p), replace=TRUE)
  sum(draws)
  })
mean(profit)# expected profit
mean(profit<0)#probability of losing money
mean(profit< -10000000)#Probability of lossing 10 Milillion or more
