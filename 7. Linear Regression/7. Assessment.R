set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
library(tidyverse)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)


head(female_heights)

#Mean of mothers' heights
m_x <- mean(female_heights$mother)
#Standard deviation of mothers' heights
s_x <- sd(female_heights$mother)

#Mean of daughters' heights
m_y <- mean(female_heights$daughter)

#Standard deviation of daughters' heights
s_y <- sd(female_heights$daughter)

#Correlation coefficient

r <-  cor(female_heights$mother, female_heights$daughter)

#Slope of regression line predicting daughters' height from mothers' heights
r <- cor(female_heights$mother, female_heights$daughter)
s_y <- sd(female_heights$daughter)
s_x <- sd(female_heights$mother)
r * s_y/s_x

#Intercept of regression line predicting daughters' height from mothers' heights

mu_y <- mean(female_heights$daughter)
mu_x <- mean(female_heights$mother)
mu_y - (r * s_y/s_x)*mu_x


#Change in daughter's height in inches given a 1 inch increase in the mother's height

r * s_y/s_x



#What percent of the variability in daughter heights is explained by the mother's height?

r^2*100

#A mother has a height of 60 inches.

#Using the regression formula, what is the conditional expected value of her daughter's height given the mother's height?

m = r * s_y/s_x
b = mu_y - (r * s_y/s_x)*mu_x
x = 60
m*x+b
