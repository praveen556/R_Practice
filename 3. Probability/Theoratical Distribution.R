library(dslabs)
data(heights)

x <- heights %>% filter(sex=="Male") %>% pull(height)

#Probability of a Male Height below 70 inches
pnorm(70,mean(x),sd(x))
#Probability of a Male Height above 70 inches
1-pnorm(70,mean(x),sd(x))

# plot distribution of exact heights in data
plot(prop.table(x), xlab = "a = Height in inches", ylab = "Pr(x=a)" )

# probabilities in actual data over length 1 ranges containing an integer
mean(x<=68.5) -  mean(x<=67.5)
mean(x<=69.5) -  mean(x<=68.5)
mean(x<=70.5) -  mean(x<=69.5)


# probabilities in normal approximities

pnorm(68.5,mean(x),sd(x))-pnorm(67.5,mean(x),sd(X))
pnorm(69.5,mean(x),sd(x))-pnorm(68.5,mean(x),sd(X))
pnorm(70.5,mean(x),sd(x))-pnorm(69.5,mean(x),sd(X))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x<=70.9) - mean(x<=70.1)
pnorm(70.9,mean(x),sd(x))-pnorm(70.1,mean(x),sd(x))