library(dslabs)
data("polls_us_election_2016")

# keep only national polls from week before election with a grade considered reliable

polls <- polls_us_election_2016 %>% filter(state=="U.S." & enddate >= "2016-10-31"
                                           & (grade %in% c("A+","A-","A","B+") | is.na(grade)))

# add spread estimate

polls <- polls %>% mutate(spread=rawpoll_clinton/100 - rawpoll_trump/100)


# collect last result before the election for each pollster
one_poll_per_pollster  <- polls %>% group_by(pollster) %>% filter(enddate==max(enddate)) %>%
  ungroup()

# histogram of spread estimates

one_poll_per_pollster %>% ggplot(aes(spread)) + geom_histogram(color="black", binwidth = 0.01)

# construct 95% confidence interval

results <- one_poll_per_pollster %>%
  summarise(avg=mean(spread), se=sd(spread)/sqrt(length(spread))) %>%
  mutate(start=avg-1.96*se, avg+1.96*se)
round(results*100,1)