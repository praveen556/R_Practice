 n <-50 
 birthday <- sample(1:365, n, replace=TRUE)
 duplicated(birthday) #To know the duplicated values out of 50
 any(duplicated(birthday)) #To know any of the persoirthday is same
 
 #Monot Carlo Simulation to repeat above step 10000 times.
 B <- 10000
 results <- replicate(B, {
   n <-50 
   birthday <- sample(1:365,n,replace = TRUE)
   any(duplicated(birthday))
 })
 mean(results)