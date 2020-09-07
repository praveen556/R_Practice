library(gtools)
library(tidyverse)

#Olympics Excersise 

medals <- permutations(8,3)
nrow(medals)

jamaica <- permutations(3,3)
nrow(jamaica)

nrow(jamaica)/nrow(medals)


set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000
all_jamaica <- replicate(B, {
  results <- sample(runners, 3)
  all(results == "Jamaica")
})
mean(all_jamaica)

#Meals Excersise  

nrow(combinations(6,1))#Entree Choice 
nrow(combinations(6,2))#Sides Choice
nrow(combinations(2,1))#Drink Choices

#How many meal combinations are possible with the current menu?
nrow(combinations(6,1))*nrow(combinations(6,2))*nrow(combinations(2,1))#Total Meal Combinations

6 * nrow(combinations(6,2)) * 2 #Another way of handling

#The manager has one additional drink he could add to the special.
nrow(combinations(6,1))*nrow(combinations(6,2))*nrow(combinations(3,1))#Total Meal Combinations

6 * nrow(combinations(6,2)) * 3 #Another way of handling

#How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?

nrow(combinations(6,1))*nrow(combinations(6,3))*nrow(combinations(3,1))#Total Meal Combinations


6 * nrow(combinations(6,3)) * 3 #Another way of handling

#The manager is concerned that customers may not want 3 sides with their meal. He is willing to increase the number of entree choices instead, but if he adds too many expensive options it could eat into profits. He wants to know how many entree choices he would have to offer in order to meet his goal.
#- Write a function that takes a number of entree choices and returns the number of meal combinations possible given that number of entree options, 3 drink choices, and a selection of 2 sides from 6 options.
#- Use sapply() to apply the function to entree option counts ranging from 1 to 12
#What is the minimum number of entree options required in order to generate more than 365 combinations?

entree_choices <- function(x){
  nrow(combinations(x,1)) * nrow(combinations(6,2)) * nrow(combinations(3,1))
}

combos <- sapply(1:12, entree_choices)

data.frame(entrees = 1:12, combos = combos) %>%
  filter(combos > 365) %>%
  min(.$entrees)


#The manager isn't sure he can afford to put that many entree choices on the lunch menu and thinks it would be cheaper for him to expand the number of sides. He wants to know how many sides he would have to offer to meet his goal of at least 365 combinations.

#- Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices, 3 drink choices, and a selection of 2 sides from the specified number of side choices.

#- Use sapply() to apply the function to side counts ranging from 2 to 12.

#What is the minimum number of side options required in order to generate more than 365 combinations?

side_choices <- function(x){
  nrow(combinations(6,1)) * nrow(combinations(x,2)) * nrow(combinations(3,1))
}

combos <- sapply(2:12, side_choices)

data.frame(sides = 2:12, combos = combos) %>%
  filter(combos > 365) %>%
  min(.$sides)


#Esophageal cancer and alcohol/tobacco use, part 1
head(esoph)
#How many groups are in the study?
nrow(esoph)
#How many cases are there?
all_cases <- sum(esoph$ncases)
all_cases
#How many controls are there?
all_controls <- sum(esoph$ncontrols)
all_controls


esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

tob_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncases) %>%
  sum()

tob_cases/all_cases

tob_controls <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncontrols) %>%
  sum()

tob_controls/all_controls

#For cases, what is the probability of being in the highest alcohol group?


alc_cases <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncases) %>%
  sum()

alc_cases/all_cases

#For cases, what is the probability of being in the highest tobacco group?


high_tob_cases <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncases) %>%
  sum()

high_tob_cases/all_cases

#For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?


high_tob_alc_cases <- esoph %>%
  filter(tobgp == "30+" & alcgp == "120+") %>%
  pull(ncases) %>%
  sum()

high_tob_alc_cases/all_cases

#For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?


high_tob_or_alc_cases <- esoph %>%
  filter(tobgp == "30+" | alcgp == "120+") %>%
  pull(ncases) %>%
  sum()

high_tob_or_alc_cases/all_cases

#For controls, what is the probability of being in the highest alcohol group?


high_alc_controls <- esoph %>%
  filter( alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()

high_alc_controls/all_controls

#How many times more likely are cases than controls to be in the highest alcohol group?


high_alc_cases <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncases) %>%
  sum()

(high_alc_cases/all_cases)/(high_alc_controls/all_controls)

#For controls, what is the probability of being in the highest tobacco group?


high_tob_controls <- esoph %>%
  filter(tobgp == "30+" ) %>%
  pull(ncontrols) %>%
  sum()

high_tob_controls/all_controls

#For controls, what is the probability of being in the highest alcohol group and the highest tobacco group?


high_tob_alc_controls <- esoph %>%
  filter(tobgp == "30+" & alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()

high_tob_alc_controls/all_controls

#For controls, what is the probability of being in the highest alcohol group or the highest tobacco group?

high_tob_or_alc_controls <- esoph %>%
  filter(tobgp == "30+" | alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()

high_tob_or_alc_controls/all_controls

#How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?

(high_tob_or_alc_cases/all_cases)/(high_tob_or_alc_controls/all_controls)
