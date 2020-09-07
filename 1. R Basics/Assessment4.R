library(dslabs)
data(heights)
s <- ifelse(heights$sex=="Female",1,2)
sum(s)

h <- ifelse(heights$height>72,heights$height,0)
h
mean(h)

inches_to_ft <- function(h){
  f <- h/12
  f
}
inches_to_ft(144)
ft <- ifelse(inches_to_ft(heights$height)<5,1,0)
ft
sum(ft)

#Define Vector with length m
m <- 10
f_n <- vector(length=m)

# Make Vector of factorials
for(n in 1:m){
  f_n[n] <- factorial(n)
}

#Check Values
f_n
