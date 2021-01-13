library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#The type column of dat indicates whether students took classes in person ("inclass") or online ("online"). What proportion of the inclass group is female? What proportion of the online group is female?

#Enter your answer as a percentage or decimal (eg "50%" or "0.50") to at least the hundredths place.
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))


#In the course videos, height cutoffs were used to predict sex. Instead of height, use the type variable to predict sex. Assume that for each class type the students are either all male or all female, based on the most prevalent sex in each class type you calculated in Q1. Report the accuracy of your prediction of sex based on type. You do not need to split the data into training and test sets.

#Enter your accuracy as a percentage or decimal (eg "50%" or "0.50") to at least the hundredths place.

y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat==y)


#Write a line of code using the table() function to show the confusion matrix between y_hat and y. 
#Use the exact format function(a, b) for your answer and do not name the columns and rows. 
#Your answer should have exactly one space. Enter the line of code below.

table(y_hat, y)

#What is the sensitivity of this prediction? You can use the sensitivity() function from the caret package. 
#Enter your answer as a percentage or decimal (eg "50%" or "0.50") to at least the hundredths place.

library(caret)
sensitivity(y_hat, y)


#What is the specificity of this prediction? You can use the specificity() function from the caret package. 
#Enter your answer as a percentage or decimal (eg "50%" or "0.50") to at least the hundredths place.

specificity(y_hat, y)

#What is the prevalence (% of females) in the dat dataset defined above? 
#Enter your answer as a percentage or decimal (eg "50%" or "0.50") to at least the hundredths place.
mean(y == "Female")

