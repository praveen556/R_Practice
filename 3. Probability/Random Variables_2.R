# define random variable x to be 1 if blue, 0 otherwise

beads <- rep(c("blue","red"), times=c(2,3))
beads
x <- ifelse(sample(beads,1)=="blue",1,0)
x #x is a random variable

