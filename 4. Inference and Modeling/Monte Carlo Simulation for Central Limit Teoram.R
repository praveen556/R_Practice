B <- 10000
N <- 1000
p <- 0.45 #Assuming p

X_Hat <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob=c(1-p,p)) #IF Dems picked then 1 and Republican picked then 0 
  mean(X)
})
mu <- mean(X_Hat)
sigma <- sd(X_Hat)

library(gridExtra)

p1 <- data.frame(X_Hat = X_Hat) %>% ggplot(aes(X_Hat))+
  geom_histogram(binwidth = 0.005 , color = "black")

p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mu, sd = sigma)) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1,p2, nrow =1)