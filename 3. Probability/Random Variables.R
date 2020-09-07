beads <- rep(c("Blue","Red"), times = c(2,3))
beads
x <- ifelse(sample(beads,1)=="Blue",1,0)
x

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "Blue", 1, 0)
ifelse(sample(beads, 1) == "Blue", 1, 0)
ifelse(sample(beads, 1) == "Blue", 1, 0)