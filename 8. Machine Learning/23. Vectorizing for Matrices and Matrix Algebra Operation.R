#We can scale each row of a matrix using this line of code:
  (x - rowMeans(x)) / rowSds(x)
#To scale each column of a matrix, we use this code:
  t(t(X) - colMeans(X))
#We can also use a function called sweep() that works similarly to apply(). It takes each entry of a vector and subtracts it from the corresponding row or column:
  X_mean_0 <- sweep(x, 2, colMeans(x))
#Matrix multiplication: t(x) %*% x
#The cross product: crossprod(x)
#The inverse of a function: solve(crossprod(x))
#The QR decomposition: qr(x)
  
  
  #scale each row of a matrix
  (x - rowMeans(x)) / rowSds(x)
  
  #scale each column
  t(t(x) - colMeans(x))
  
  #take each entry of a vector and subtracts it from the corresponding row or column
  x_mean_0 <- sweep(x, 2, colMeans(x))
  
  #divide by the standard deviation
  x_mean_0 <- sweep(x, 2, colMeans(x))
  x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")