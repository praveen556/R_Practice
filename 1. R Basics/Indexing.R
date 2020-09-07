library(dslabs)
data("murders")

murder_rate <- murders$total*100000/murders$population
#To get the safe_states
safe_states_index <- murder_rate <= 1
#Get the number of safe States
sum(safe_states_index)
#To Get the State Names
murders$state[safe_states_index]
#TO get the states in the West
West <- murders$region == "West"

murders$state[safe_states_index & West]