#What is the expected value of the payout for one bet?
p<-5/38
p

a <-6
b <--1
n<-500

mu <- (a*p)+(b*(1-p))
mu

#What is the standard error of the payout for one bet?
sigma <-  abs(b-a) *sqrt(p*(1-p))
sigma

#What is the expected value of the average payout over 500 bets?
mu

#What is the standard error of the average payout over 500 bets?
sigma/sqrt(n)

#What is the expected value of the sum of 500 bets?
n*mu

#What is the standard error of the sum of 500 bets?
sqrt(n)*sigma

#Use pnorm() with the expected value of the sum and standard error of the sum to calculate the probability of losing money over 500 bets,  
pnorm(0,n*mu,sqrt(n)*sigma)
