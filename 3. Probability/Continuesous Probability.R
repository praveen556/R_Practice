library(tidyverse)
library(dslabs)
x <- heights %>% filter(sex=="Male") %>% pull(height)
F <- function(a)mean(x<=a)
1 - F(70) # probability of male taller than 70 inches

#We can estimate the probability that a male is taller than 70.5 inches using
1 - pnorm(70.5, mean(x), sd(x))

# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)

pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

#Plotting Probability Density

x <- seq(-4,4, length =100 )
x
data.frame(x, f=dnorm(x))%>%
  ggplot(aes(x,f))+
  geom_line()
#with Mean and Standard Deviation
data.frame(x, f=dnorm(x,mean(x),sd(x)))%>%
  ggplot(aes(x,f))+
  geom_line()

#Generating normally distributed random numbers

x<- heights %>% filter(sex=="Male")%>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)

simulated_heights <- rnorm(n,avg,s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights)%>%
  ggplot(aes(simulated_heights))+
  geom_histogram(color="black",binwidth = 2)

#Monte Carlo simulation of tallest person over 7 feet
B <- 10000

tallest <- replicate(B, {
  simulated_data <- rnorm(800,avg,s)# generate 800 normally distributed random heights
  max(simulated_data)# determine the tallest height
})
mean(tallest>=7*12)


#Plotting the normal distribution with dnorm

x <- seq(-4,4, length.out = 100)

data.frame(x, f=dnorm(x)) %>%
             ggplot(aes(x,f))+
             geom_line()
