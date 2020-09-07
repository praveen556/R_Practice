#Validating normal operations on Vectors
num_samp <- seq(1,10)
num_samp2 <- seq(101,110)
num_samp*num_samp2
num_samp*2
sqrt(num_samp)
sapply(num_samp,sqrt)

#sapply on Birthday Problem
n<-60
bth <- sample(1:365,n,replace = TRUE)
any(duplicated(bth))

#Function to calculate probability of shared bdays across n people
compute_prob <- function(n,B=10000){
  same_day <- replicate(B, {
    bth <- sample(1:365,n,replace = TRUE)
    any(duplicated(bth))
  })
  mean(same_day)
}

n <-seq(1,60)
#Probability based on Monto Carlo
prob <- sapply(n,compute_prob)

plot(n,prob)

#Calculate Exact Probability 

exact_prob <- function(n){
  prob_uniq <- seq(365,365-n+1)/365 # vector of fractions for mult. rule
  1-prod(prob_uniq) # calculate prob of no shared birthdays and subtract from 1
}
#Probability based on Mathametic function

eprob <- sapply(n,exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob
