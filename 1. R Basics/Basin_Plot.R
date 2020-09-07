x <- murders$population/10^6
y <- murders$total
plot(x,y)
#histogram
hist(murders$rate)
#Boxplots
boxplot(rate~region, data=murders)
