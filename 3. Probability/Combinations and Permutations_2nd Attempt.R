# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants=c("blue","black"),shirts =c("white","plaid","grey"))

#Generating a deck of cards
suits <- c("Hearts","Clubs","Spades","Diamonds")
numbers <- c("Aces","Two","Three","Four","Five","Six","Seven","Eight","Nine","Ten","Jacks","Queens","Kings")
deck <- expand.grid(suits=suits,numbers =numbers)
deck <- paste(deck$numbers,deck$suits)
# probability of drawing a king
kings <- paste("Kings", suits)
mean(deck %in% kings)

#Permutations and combinations
library(gtools)
permutations(5,2) # ways to choose 2 numbers in order from 1:5


all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matt


#Probability of drawing a second king given that one king is drawn
hands <- permutations(52,2,v=deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

#Probability of a natural 21 in blackjack
aces <- paste("Aces", suits)
facecard <- c("Kings", "Queens", "Jacks", "Tens")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

#Monte Carlo simulation of natural 21 in blackjack

# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)
