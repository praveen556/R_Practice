#In a previous module, we covered Bayes' theorem and the Bayesian paradigm. Conditional probabilities are a fundamental part of this previous covered rule.

#P(A|B)=P(B|A)P(A)P(B) 
#We first review a simple example to go over conditional probabilities.

#Assume a patient comes into the doctor’s office to test whether they have a particular disease.

#The test is positive 85% of the time when tested on a patient with the disease (high sensitivity):  P(test+|disease)=0.85 
#The test is negative 90% of the time when tested on a healthy patient (high specificity):  P(test−|heathy)=0.90 
#The disease is prevalent in about 2% of the community:  P(disease)=0.02 
#Using Bayes' theorem, calculate the probability that you have the disease if the test is positive.

(0.85*0.02)/((0.85*0.02)+(0.1*0.98))

#The following 4 questions (Q2-Q5) all relate to implementing this calculation using R.

#We have a hypothetical population of 1 million individuals with the following conditional probabilities as described below:
  
#  The test is positive 85% of the time when tested on a patient with the disease (high sensitivity):  P(test+|disease)=0.85 
#The test is negative 90% of the time when tested on a healthy patient (high specificity):  P(test−|heathy)=0.90 
#The disease is prevalent in about 2% of the community:  P(disease)=0.02 
#Here is some sample code to get you started:

set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#What is the probability that a test is positive?
mean(test)

#What is the probability that an individual has the disease if the test is negative?
mean(disease[test==0])

#What is the probability that you have the disease if the test is positive?
mean(disease[test==1])


#Compare the prevalence of disease in people who test positive to the overall prevalence of disease.

#If a patient's test is positive, by how many times does that increase their risk of having the disease?

mean(disease[test==1]==1)/mean(disease==1)


#We are now going to write code to compute conditional probabilities for being male in the heights dataset. Round the heights to the closest inch. Plot the estimated conditional probability  P(x)=Pr(Male|height=x)  for each  x .

#Part of the code is provided here:
  
  
library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

#In the plot we just made in Q6 we see high variability for low values of height. This is because we have few data points. This time use the quantile  0.1,0.2,…,0.9  and the cut() function to assure each group has the same number of points. Note that for any numeric vector x, you can create groups based on quantiles like this: cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE).

#Part of the code is provided here:

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#You can generate data from a bivariate normal distrubution using the MASS package using the following code:
#install.packages("MASS")

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#And you can make a quick plot using plot(dat).

#Using an approach similar to that used in the previous exercise, let's estimate the conditional expectations and make a plot. Part of the code has again been provided for you:

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)
