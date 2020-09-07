library(dslabs)
data(heights)
tab <- table(heights$height)
sum(tab==1)


prop.table(table(heights$height))

my_data <- heights$height

min(my_data)
max(my_data)
a <- seq(min(my_data),max(my_data), length=100)
a
cdf_function <- function(x){
  mean(my_data <= x)
}

cdf_values <- sapply(a, cdf_function)
sapply(a, cdf_function)
plot(a, cdf_values)