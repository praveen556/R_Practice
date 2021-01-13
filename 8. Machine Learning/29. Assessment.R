#Previously, we used logistic regression to predict sex based on height. Now we are going to use knn to do the same. Set the seed to 1, then use the caret package to partition the dslabs heights data into a training and test set of equal size. Use the sapply() function to perform knn with k values of seq(1, 101, 3) and calculate F1 scores with the F_meas() function using the default value of the relevant argument.

data("heights")

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
## Warning in set.seed(1, sample.kind = "Rounding"): non-uniform 'Rounding' sampler
## used
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)

#What is the max value of F_1?
max(F_1)

#At what value of k does the max occur?
ks[which.max(F_1)]

#Next we will use the same gene expression example used in the Comprehension Check: Distance exercises. You can load it like this:

library(dslabs)
library(caret)
data("tissue_gene_expression")

#First, set the seed to 1 and split the data into training and test sets with p = 0.5. Then, report the accuracy you obtain from predicting tissue type using KNN with k = seq(1, 11, 2) using sapply() or map_df(). Note: use the createDataPartition() function outside of sapply() or map_df().

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})
