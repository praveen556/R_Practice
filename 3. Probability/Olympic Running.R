#How many different ways can the 3 medals be distributed across 8 runners?
library(gtools)
permutations(8,3)
#get the total number of rows

#How many different ways can the three medals be distributed among the 3 runners from Jamaica?
permutations(3,3)
#get the total number of rows

#What is the probability that all 3 medals are won by Jamaica?
(1/8)*(2/7)*(3/6)

#Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race:

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <-10000 
set.seed(1)

ran <- as.character(1:8)

runner <- replicate(B,{
  combinations()
  hand <- samples(runners,3)
  (hand[1] %in% "Jamaica" & hand[2] %in% "Jamaica"& hand[3] %in% "Jamaica")
})
mean(results)