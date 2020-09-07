compute_s_n <- function(i){
  x <- 1:n
  sum(x)
}
m <- 25
s_n <- vector(length=m)

for(n in 1:m){
  s_n[n]<-compute_s_n(n)
}

#creating plot for summation function
n <- 1:m

plot(n, s_n)

#atable of values for comparing our function for the summation formula
head(data.frame(s_n=s_n, formula=n*(n+1)/2))

#Overlaying our function with the summation formula
plot(n,s_n)
lines(n, n*(n+1)/2)

compute_s_n <- function(n){
  for(m in 1:n){
    y <- data.frame(m^2)
    print(y)
  }
  y <- sum(1:n)
  y
}

# Report the value of the sum when n=10
compute_s_n(10)