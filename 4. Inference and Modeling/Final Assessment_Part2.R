#Brexit poll analysis - Part 2

# suggested libraries
library(tidyverse)
# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)
# final proportion voting "Remain"
p <- 0.481

#Question 4: Confidence intervals for polls in June
#How many polls are in june_polls?
june_polls <- brexit_polls %>%
  filter(enddate >= "2016-06-01")
nrow(june_polls)
#What proportion of polls have a confidence interval that covers the value 0?
d = -0.038
june_polls <- brexit_polls %>% filter(enddate >="2016-06-01") %>% 
  mutate(se_x_hat = 2 * sqrt(x_hat * (1-x_hat)/samplesize)) %>% 
  mutate(lower=spread - qnorm(0.975)*se_x_hat, upper=spread + qnorm(0.975)*se_x_hat) %>% 
  mutate(hit = d <=upper & d >=lower)
june_polls %>% summarize(n=n())
mean(june_polls$lower<=0 & june_polls$upper>=0)


#What proportion of polls predict "Remain" (confidence interval entirely above 0)?
ind <- june_polls$lower >= 0
mean(ind)

#What proportion of polls have a confidence interval covering the true value of  ???? ?
ind <- june_polls$hit
mean(ind)

#Question 5: Hit rate by pollster
#Group and summarize the june_polls object by pollster to find the proportion of hits 
#for each pollster and the number of polls per pollster. Use arrange() to sort by hit rate.
x <- june_polls %>% group_by(pollster) %>% summarize(proportion_hits=mean(hit), n=n()) %>% 
  arrange(desc(proportion_hits))
x
#Unbiased polls and pollsters will theoretically cover the correct value of the spread 50% of the time.
#The results are consistent with a large general bias that affects all pollsters. correct


#Question 6: Boxplot of Brexit polls by poll type
#Make a boxplot of the spread in june_polls by poll type.
june_polls %>% ggplot(aes(group=poll_type, x=poll_type,  y=spread)) + geom_boxplot() + geom_point()

#Telephone polls tend to show support "Remain" (spread > 0).
#Telephone polls tend to show higher support for "Remain" than online polls (higher spread).
#Online polls have a larger interquartile range (IQR) for the spread than telephone polls, indicating that they are more variable.
#Poll type introduces a bias that affects poll results.

#Question 7: Combined spread across poll type
#Calculate the confidence intervals of the spread combined across all polls in june_polls, grouping by poll type.
#Use this code to begin your analysis:
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)
#What is the lower bound of the 95% confidence interval for online voters?
type <- combined_by_type %>% mutate(lower_bound=spread - qnorm(0.975)*2 * sqrt(p_hat * (1-p_hat)/N), 
                                    upper_bound=spread + qnorm(0.975)*2 * sqrt(p_hat * (1-p_hat)/N))
type %>%
  filter(poll_type == "Online") %>%
  select(lower_bound)
#What is the upper bound of the 95% confidence interval for online voters?
type %>%
  filter(poll_type == "Online") %>%
  select(upper_bound)
#Interpret the confidence intervals for the combined spreads for each poll type calculated in the previous problem.

#Which of the following are TRUE about the confidence intervals of the combined spreads for different poll types?
type %>%
  filter(poll_type == "Telephone") %>%
  select(lower_bound)
#What is the upper bound of the 95% confidence interval for online voters?
type %>%
  filter(poll_type == "Telephone") %>%
  select(upper_bound)

#Neither set of combined polls makes a prediction about the outcome of the Brexit referendum (a prediction is possible if a confidence interval does not cover 0).
#The confidence interval for telephone polls is covers more positive values than the confidence interval for online polls.
#Neither confidence interval covers the true value of  d=???0.038 .