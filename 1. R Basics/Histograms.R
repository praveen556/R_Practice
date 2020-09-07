library(dslabs)
library(tidyverse)
data(heights)

#Male Murders into P

p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x=height))

p + geom_histogram()   #Basic Histogram
p + geom_histogram(binwidth=1)

# histogram with blue fill, black outline, labels and title

p + geom_histogram( binwidth = 1, fill = "blue", col = "black")+
  xlab("Male Heights in Inches")+
  ggtitle("Histogram")

#smooth Density plot

p + geom_density()
p + geom_density(fill ="blue")

#Quantile-quantile(QQ) plots in ggplot

p <- heights %>% filter(sex == "Male") %>% ggplot(aes(sample=height))
p + geom_qq()

# QQ-plot against a normal distribution with same mean/sd as data
parems <- heights %>% filter(sex == "Male") %>% summarize(mean=mean(height), sd=sd(height))
p + geom_qq(dparams = parems) +
  geom_abline()


# QQ-plot of scaled data against the standard normal distribution

heights %>% ggplot(aes(sample = scale(height)))+
  geom_qq()+
  geom_abline()

# define plots p1, p2, p3

p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x=height))

p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")
p4 <- p + geom_histogram(binwidth = 4, fill = "blue", col = "black")

#Arranging graphs in a grid
library(gridExtra)
grid.arrange(p1,p2,p3, ncol = 3)

#Arranging in square box
grid.arrange(p1,p2,p3,p4, ncol =2, nrow = 2)
