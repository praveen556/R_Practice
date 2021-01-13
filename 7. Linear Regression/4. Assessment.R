library(Lahman)
data(Teams)

head(Teams)

#Correlation Coefficient
set.seed(1)
#rho <- mean(scale(x)*scale(y))
#Load the Lahman library. Filter the Teams data frame to include years from 1961 to 2001.
#What is the correlation coefficient between number of runs per game and number of at bats per game?

library(Lahman)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
cor(Teams_small$R/Teams_small$G, Teams_small$AB/Teams_small$G)

#Use the filtered Teams data frame from Question 7.

#What is the correlation coefficient between win rate (number of wins per game) and number of errors per game?

Teams %>% filter(yearID %in% 1961:2001) %>% summarize(r = cor(E/G, W/G)) %>% pull(r)

#Use the filtered Teams data frame from Question 7.

#What is the correlation coefficient between doubles (X2B) per game and triples (X3B) per game?

Teams %>% filter(yearID %in% 1961:2001) %>% summarize(r = cor(X3B/G, X2B/G)) %>% pull(r)
  
