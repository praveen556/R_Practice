library(tidyverse)
library(dslabs)
data(heights)

x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations

mu <- mean(x)
segma <- sd(x)
n <- length(x)

simulated_height <- rnorm(n,mu,segma)

# plot distribution of simulated_heights

data.frame(simulated_height = simulated_height) %>%
  ggplot(aes(simulated_height))+
  geom_histogram(color="black", binwidth = 2)


#Monte Carlo simulation of tallest person over 7 feet

B <-10000

simulated_heights_monto <-replicate(B,{
  simulated_data_monto <- rnorm(800,mu,sigma)
  max(simulated_data_monto)
})
mean(simulated_heights_monto>=72)
