#Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when the degrees of freedom are 3.

# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.
1-pt(2,3)+pt(-2,3)

#Now use sapply to compute the same probability for degrees of freedom from 3 to 50.

#Make a plot and notice when this probability converges to the normal distribution's 5%.

# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df <- seq(3,50,1)

df
# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 

pt_func <- function(n){
  1 - pt(2,n)+pt(-2,n)
}

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities

probs <- sapply(df, pt_func)


# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df,probs)


#In a previous section, we repeatedly took random samples of 50 heights from a distribution of heights. We noticed that about 95% of the samples had confidence intervals spanning the true population mean.

#Re-do this Monte Carlo simulation, but now instead of N=50, use N=15. Notice what happens to the proportion of hits.
# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qnorm(0.975)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)


#N=15 is not that big. We know that heights are normally distributed, so the t-distribution should apply. Repeat the previous Monte Carlo simulation using the t-distribution instead of using the normal distribution to construct the confidence intervals.
#What are the proportion of 95% confidence intervals that span the actual mean height now?
 
# The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
mu <- mean(x)

# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution
res <- replicate(B, {
  s <- sample(x, N, replace = TRUE)
  interval <- c(mean(s) - qt(0.975, N - 1) * sd(s) / sqrt(N), mean(s) + qt(0.975, N - 1) * sd(s) / sqrt(N))
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res) 