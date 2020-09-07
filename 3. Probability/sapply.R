# function to calculate probability of shared bdays across n people

compute_prob <- function(n, B=10000){
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace=TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
} 

n <- seq(1,60)

#Element-wise operation over vectors and sapply

x <- seq(1,10)
sqrt(x) # sqrt operates on each element of the vector

y <- 1:10
sqrt(y)

x*y # * operates element-wise on both vectors

sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob) # element-wise application of compute_prob to n
prob
plot(n,prob) #plotting simple graph

# function for computing exact probability of shared birthdays for any n

exact_prob <- function(n) {
 prob_uniq <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
 1-prod(prob_uniq) # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values

eprob <- sapply(n, exact_prob)


# plotting Monte Carlo results and exact probabilities on same graph
plot(n,prob)
lines(n,eprob, col="red")


#3. A and B play a series - part 1
# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}

# Apply the 'prob_win' function across the vector of probabilities that team A will win to determine the probability that team B will win. Call this object 'Pr'.
Pr <-sapply(p, prob_win)

# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p,Pr)

#4. A and B play a series - part 2

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}

# Assign the variable 'N' as the vector of series lengths. Use only odd numbers ranging from 1 to 25 games.
N <- seq(1, 25, 2)

# Apply the 'prob_win' function across the vector of series lengths to determine the probability that team B will win. Call this object `Pr`.
Pr<- sapply(N, prob_win)

# Plot the number of games in the series 'N' on the x-axis and 'Pr' on the y-axis.
plot(N, Pr)

