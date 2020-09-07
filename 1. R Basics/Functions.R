my_first_function <- function(x){
  s <-sum(x)
  y <- length(x)
  s/y
}

arg <- 1:100
my_first_function(arg)

identical(mean(arg),my_first_function(arg))

avg <- function(x, arithmetic=TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

avg(1:10,FALSE)
avg(1:10,TRUE)