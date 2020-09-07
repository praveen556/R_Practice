X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
pnorm(0.01/se) - pnorm(-0.01/se)

pnorm(2) - pnorm(-2) #Chance of Expected values of X will be within 95%, Margin of error is 2 times
