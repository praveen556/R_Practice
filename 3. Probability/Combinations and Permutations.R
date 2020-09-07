suits <- c("Diamond","Hearts","Spades","Clubs")
numbers <- c("Ace","Duece","Three","Four","Five","Six","Seven","Eight","Nine","Ten","Jack","Queen","King")
deck <- expand.grid(number=numbers,suit=suits) #Command to generate the permutation for each  combination
deck <- paste(deck$number,deck$suit) #concating numbers and suits
deck

#Check probablity of getting a king
kings <- paste("King",suits)  #Adding King string to Suit variable
kings
mean(deck %in% kings) #Checking probability for getting king

library(gtools)
permutations(5,2)

all_phonenumbers <- permutations(10,7, v=0:9)
n <- nrow(all_phonenumbers)
index <- sample(n,5)
index
all_phonenumbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

#Check probability of getting second card king
hands <- permutations(52,2,v=deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

#Probability of a natural 21 in blackjack

aces <- paste("Ace",suits)

facecard <- c("King","Queen","Jack","Ten")

facecard <- expand.grid(number=facecard, suit=suits)

facecard <- paste(facecard$number, facecard$suit)

hands <- permutations(52,2,v=deck)#All Possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,1] %in% facecard & hands[,2] %in% aces))

#Monte Carlo simulation of natural 21 in blackjack
hand <- sample(deck,2)
hand

# code for B=10,000 hands of blackjack
b <- 10000
samp_size <- replicate(b,{
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(samp_size)
