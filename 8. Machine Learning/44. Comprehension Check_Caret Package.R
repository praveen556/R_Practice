#Load the rpart package and then use the caret::train() function with method = "rpart" to fit a classification tree to the tissue_gene_expression dataset. Try out cp values of seq(0, 0.1, 0.01). Plot the accuracies to report the results of the best model. Set the seed to 1991.
  
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
data("tissue_gene_expression")

fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit)

#Which value of cp gives the highest accuracy?

##**0**##

#Note that there are only 6 placentas in the dataset. By default, rpart requires 20 observations before splitting a node. That means that it is difficult to have a node in which placentas are the majority. Rerun the analysis you did in Q1 with caret::train(), but this time with method = "rpart" and allow it to split any node by using the argument control = rpart.control(minsplit = 0). Look at the confusion matrix again to determine whether the accuracy increases. Again, set the seed to 1991.

set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))
ggplot(fit_rpart)

#What is the accuracy now?
confusionMatrix(fit_rpart)

#Plot the tree from the best fitting model of the analysis you ran in Q2.

#Which gene is at the first split?
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

##**GPA33**##

#We can see that with just seven genes, we are able to predict the tissue type. Now let's see if we can predict the tissue type with even fewer genes using a Random Forest. Use the train() function and the rf method to train a Random Forest model and save it to an object called fit. Try out values of mtry ranging from seq(50, 200, 25) (you can also explore other values on your own). What mtry value maximizes accuracy? To permit small nodesize to grow as we did with the classification trees, use the following argument: nodesize = 1.

#Note: This exercise will take some time to run. If you want to test out your code first, try using smaller values with ntree. Set the seed to 1991 again.

#What value of mtry maximizes accuracy?
##**100**##

set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
library(randomForest)
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

ggplot(fit)


#Use the function varImp() on the output of train() and save it to an object called imp:

imp <- varImp(fit)#BLANK
imp

#The rpart() model we ran above in Q2 produced a tree that used just seven predictors. Extracting the predictor names is not straightforward, but can be done. If the output of the call to train was fit_rpart, we can extract the names like this:


tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

#Calculate the variable importance in the Random Forest call from Q4 for these seven predictors and examine where they rank.

#What is the importance of the CFHR4 gene in the Random Forest call?
#What is the rank of the CFHR4 gene in the Random Forest call?

data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)
