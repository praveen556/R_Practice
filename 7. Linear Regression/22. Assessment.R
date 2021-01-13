#Use the Teams data frame from the Lahman package. Fit a multivariate linear regression model to obtain the effects of BB and HR on Runs (R) in 1971. Use the tidy() function in the broom package to obtain the results in a data frame.

#What is the estimate for the effect of BB on runs?


Teams %>%
  filter(yearID == 1971) %>%
  lm(R ~ BB + HR, data = .) %>%
  tidy() %>%
  filter(term == "BB") %>%
  pull(estimate)

#What is the estimate for the effect of HR on runs?
Teams %>%
  filter(yearID == 1971) %>%
  lm(R ~ BB + HR, data = .) %>%
  tidy() %>%
  filter(term == "HR") %>%
  pull(estimate)



#Interpret the p-values for the estimates using a cutoff of 0.05.

#Which of the following is the correct interpretation?
  

fit <- Teams %>% filter(yearID %in% 1971) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)

tidy(fit, conf.int = TRUE)


#Repeat the above exercise to find the effects of BB and HR on runs (R) for every year from 1961 to 2018 using do() and the broom package.

#Make a scatterplot of the estimate for the effect of BB on runs over time and add a trend line with confidence intervals.

#Fill in the blank to complete the statement:

res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup() 
res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")


#The effect of BB on runs has 
##Select an option *Increased*
#unanswered  over time.

#Fit a linear model on the results from Question 10 to determine the effect of year on the impact of BB.

#For each additional year, by what value does the impact of BB on runs change?

res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") %>%
  pull(estimate)

#What is the p-value for this effect?
res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") %>%
  pull(p.value)

  

