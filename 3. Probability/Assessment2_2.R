library(tidyverse)
library(dslabs)
set.seed(16)
act_scores<-rnorm(10000,20.9,5.7)
mean(act_scores)
sd(act_scores)

sum(act_scores>=36)#To get the number of people perfect score of 36 or more

1-pnorm(30,mean(act_scores),sd(act_scores)) #what is the probability of an ACT score greater than 30?

#what is the probability of an ACT score less than or equal to 10?
pnorm(10,mean(act_scores),sd(act_scores)) - pnorm(0,mean(act_scores),sd(act_scores))
#OR
mean(act_scores <= 10)

x <- seq(1,36)

f_x <- dnorm(x,20.9,5.7)
plot(x,f_x)

#OR
data.frame(x, f_x) %>%
  ggplot(aes(x, f_x)) +
  geom_line()


Z_Scores <- (act_scores - mean(act_scores))/sd(act_scores)
#What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?

1-pnorm(2,mean(Z_Scores),sd(Z_Scores))
#OR
mean(Z_Scores > 2)

#What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
2*sd(act_scores) + mean(act_scores)

#A Z-score of 2 corresponds roughly to the 97.5th percentile.
qnorm(0.975,mean(act_scores), sd(act_scores))

#Write a function that takes a value and produces the probability of an ACT score less than or equal to 
#that value (the CDF). Apply this function to the range 1 to 36.

cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
min(which(cdf >= .95))

#What is the expected 95th percentile of ACT scores
qnorm(.95,20.9,5.7)

#In what percentile is a score of 26?
p <- seq(0.01, 0.99, 0.01)
sample_quantiles<-quantile(act_scores, p)
names(sample_quantiles[max(which(sample_quantiles < 26))])

#Make a corresponding set of theoretical quantiles using qnorm() over the interval p <- seq(0.01, 0.99, 0.01) 
#with mean 20.9 and standard deviation 5.7. Save these as theoretical_quantiles. 
#Make a QQ-plot graphing sample_quantiles on the y-axis versus theoretical_quantiles on the x-axis.

#Which of the following graphs is correct?
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()
