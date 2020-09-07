cyan <- 3
magenta <- 5
yellow <- 7

# Assign a variable `p` as the probability of choosing a cyan ball from the box
p <- mean(rep(c("cyan","magenta","yellow"), times=c(3,5,7))=="cyan")

# Print the variable `p` to the console
p
