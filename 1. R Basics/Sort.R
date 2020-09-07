library(dslabs)
data(murders)
murders
sort(murders$total)
Index <- order(murders$total, decreasing = TRUE)
Index
murders$state[Index]
max(murders$total)
murders$state[which.max(murders$total)]
rank(murders$total)
murders$state[rank(murders$total)]
