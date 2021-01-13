#Which line of code correctly creates a 100 by 10 matrix of randomly generated normal numbers and assigns it to x?

x <- matrix(rnorm(100*10), 100, 10)


#Write the line of code that would give you the specified information about the matrix x that you generated in q1. Do not include any spaces in your line of code.

#Dimension of x
dim(x)

#Number of rows of x
nrow(x) 
#or
dim(x)[1] 
#or 
length(x[,1])
#Number of columns of x
ncol(x)
or
dim(x)[2]
or
length(x[1,])


#Which of the following lines of code would add the scalar 1 to row 1, the scalar 2 to row 2, and so on, for the matrix x?


x <- x + seq(nrow(x))
or
x <- sweep(x, 1, 1:nrow(x),"+")


#Which of the following lines of code would add the scalar 1 to column 1, the scalar 2 to column 2, and so on, for the matrix x?

x <- sweep(x, 2, 1:ncol(x), FUN = "+")

#Which code correctly computes the average of each row of x?

rowMeans(x)

#Which code correctly computes the average of each column of x?
colMeans(x)

#For each observation in the mnist training data, compute the proportion of pixels that are in the grey area, defined as values between 50 and 205 (but not including 50 and 205). (To visualize this, you can make a boxplot by digit class.)

#What proportion of the 60000*784 pixels in the mnist training data are in the grey area overall, defined as values between 50 and 205? Report your answer to at least 3 significant digits.

mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")


mean(y)
