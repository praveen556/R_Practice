#For each poll in the polling data set, use the CLT to create a 95% confidence interval for the spread. Create a new table called cis that contains columns for the lower and upper limits of the confidence intervals.


## Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the instructions
cis <- polls %>% mutate(X_hat = (spread+1)/2, se = 2*sqrt(X_hat*(1-X_hat)/samplesize), 
                        lower = spread - qnorm(0.975)*se, upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)



#You can add the final result to the cis table you just created using the left_join function as shown in the sample code.
#Now determine how often the 95% confidence interval includes the actual result.

# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.
p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% summarize(proportion_hits = mean(hit))
p_hits




#Now find the proportion of hits for each pollster. Show only pollsters with more than 5 polls and order them from best to worst. Show the number of polls conducted by each pollster and the FiveThirtyEight grade of each pollster.
# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls. 
p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% 
  group_by(pollster) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n(), grade = grade[1]) %>%
  arrange(desc(proportion_hits))


#Repeat the previous exercise, but instead of pollster, stratify by state. Here we can't show grades.

# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls.

p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% 
  group_by(state) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n()) %>%
  arrange(desc(proportion_hits)) 
p_hits


#Make a barplot based on the result from the previous exercise.

# The `p_hits` data have already been loaded for you. Use the `head` function to examine it.
head(p_hits)

# Make a barplot of the proportion of hits for each state

p_hits %>% mutate(state = reorder(state, proportion_hits)) %>%
  ggplot(aes(state, proportion_hits)) + 
  geom_bar(stat = "identity") +
  coord_flip()


#Even if a forecaster's confidence interval is incorrect, the overall predictions will do better if they correctly called the right winner.

#Add two columns to the cis table by computing, for each poll, the difference between the predicted spread and the actual spread, and define a column hit that is true if the signs are the same.

# The `cis` data have already been loaded. Examine it using the `head` function.

cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
head(cis)

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted

errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Examine the last 6 rows of `errors`
tail(errors)


#Create an object called p_hits that contains the proportion of instances when the sign of the actual spread matches the predicted spread for states with 5 or more polls.

#Make a barplot based on the result from the previous exercise that shows the proportion of times the sign of the spread matched the actual result for the data in p_hits.

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls
p_hits <- errors %>%  group_by(state) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n())

# Make a barplot of the proportion of hits for each state
p_hits %>% mutate(state = reorder(state, proportion_hits)) %>%
  ggplot(aes(state, proportion_hits)) + 
  geom_bar(stat = "identity") +
  coord_flip()

#In the previous graph, we see that most states' polls predicted the correct winner 100% of the time. Only a few states polls' were incorrect more than 25% of the time. Wisconsin got every single poll wrong. In Pennsylvania and Michigan, more than 90% of the polls had the signs wrong.

#Make a histogram of the errors. What is the median of these errors?

hist(errors$error)

# Calculate the median of the errors. Print this value to the console.
median(errors$error)

#We see that, at the state level, the median error was slightly in favor of Clinton. The distribution is not centered at 0, but at 0.037. This value represents the general bias we described in an earlier section.

#Create a boxplot to examine if the bias was general to all states or if it affected some states differently. Filter the data to include only pollsters with grades B+ or higher.

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c("A+","A","A-","B+") | is.na(grade)) %>%
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(state, error)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_boxplot() + 
  geom_point()

#Some of these states only have a few polls. Repeat the previous exercise to plot the errors for each state, but only include states with five good polls or more.

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for states with at least 5 polls with grades B+ or higher
errors %>% filter(grade %in% c("A+","A","A-","B+") | is.na(grade)) %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(state, error)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_boxplot() + 
  geom_point()