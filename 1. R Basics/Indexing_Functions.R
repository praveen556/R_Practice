which(murders$state=="California")
index1 <- match(c("Florida","California","New York"),murders$state)
index1
index2 <- c("Florida","St.Johns","Miami")
index2 %in% murders$state
