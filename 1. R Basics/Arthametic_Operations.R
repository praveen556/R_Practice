library(dslabs)
data(murders)
murder_rate <- (murders$total/murders$population)*100000
murders$state[order(murder_rate, decreasing = TRUE)]


name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
rn_df <- data.frame(name, distance, time)
speed <- (rn_df$distance)*60/rn_df$time
speed