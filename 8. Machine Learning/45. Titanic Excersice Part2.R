-#Set the seed to 1. Train a model using linear discriminant analysis (LDA) with the caret lda method using fare as the only predictor.
  
  #What is the accuracy on the test set for the LDA model?
  set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
lda_preds <- predict(train_lda, test_set)
mean(lda_preds == test_set$Survived)

#Set the seed to 1. Train a model using quadratic discriminant analysis (QDA) with the caret qda method using fare as the only predictor.

#What is the accuracy on the test set for the QDA model?

train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
qda_preds <- predict(train_qda, test_set)
mean(qda_preds == test_set$Survived)

#Set the seed to 1. Train a logistic regression model with the caret glm method using age as the only predictor.

#What is the accuracy of your model (using age as the only predictor) on the test set ?

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later

train_glm_age <- train(Survived ~ Age, method = "glm", data = train_set)
glm_preds_age <- predict(train_glm_age, test_set)
mean(glm_preds_age == test_set$Survived)

#Set the seed to 1. Train a logistic regression model with the caret glm method using four predictors: sex, class, fare, and age.

#What is the accuracy of your model (using these four predictors) on the test set?

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later

train_glm <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
glm_preds <- predict(train_glm, test_set)
mean(glm_preds == test_set$Survived)

#Set the seed to 1. Train a logistic regression model with the caret glm method using all predictors. Ignore warnings about rank-deficient fit.

#What is the accuracy of your model (using all predictors) on the test set ?

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
glm_all_preds <- predict(train_glm_all, test_set)
mean(glm_all_preds == test_set$Survived)


#Set the seed to 6. Train a kNN model on the training set using the caret train function. Try tuning with k = seq(3, 51, 2).

#What is the optimal value of the number of neighbors k?

set.seed(6, sample.kind = "Rounding") # if using R 3.6 or later

train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune

#Plot the kNN model to investigate the relationship between the number of neighbors and accuracy on the training set.

#Of these values of  k , which yields the highest accuracy?

ggplot(train_knn)


#What is the accuracy of the kNN model on the test set?

knn_preds <- predict(train_knn, test_set)
mean(knn_preds == test_set$Survived)

#Set the seed to 8 and train a new kNN model. Instead of the default training control, use 10-fold cross-validation where each partition consists of 10% of the total. Try tuning with k = seq(3, 51, 2).

#What is the optimal value of k using cross-validation?

set.seed(8, sample.kind = "Rounding")    # simulate R 3.5

train_knn_cv <- train(Survived ~ .,
                      method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
train_knn_cv$bestTune

#What is the accuracy on the test set using the cross-validated kNN model?
knn_cv_preds <- predict(train_knn_cv, test_set)
mean(knn_cv_preds == test_set$Survived)

#Set the seed to 10. Use caret to train a decision tree with the rpart method. Tune the complexity parameter with cp = seq(0, 0.05, 0.002).

#What is the optimal value of the complexity parameter (cp)?

set.seed(10, sample.kind = "Rounding")    # simulate R 3.5

train_rpart <- train(Survived ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set)
train_rpart$bestTune

#What is the accuracy of the decision tree model on the test set?

rpart_preds <- predict(train_rpart, test_set)
mean(rpart_preds == test_set$Survived)


#Inspect the final model and plot the decision tree.

#Which variables are used in the decision tree?

train_rpart$finalModel # inspect final model
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel)

#Using the decision rules generated by the final model, predict whether the following individuals would survive.

##**A 28-year-old male would NOT survive
##**A female in the second passenger class would survive
##**A third-class female who paid a fare of $8 would survive
##**A 5-year-old male with 4 siblings would NOT survive
##**A third-class female who paid a fare of $25 would NOT survive
##**A first-class 17-year-old female with 2 siblings would survive
##**A first-class 17-year-old male with 2 siblings would NOT survive

#Set the seed to 14. Use the caret train() function with the rf method to train a random forest. Test values of mtry = seq(1:7). Set ntree to 100.

#What mtry value maximizes accuracy?

set.seed(14, sample.kind = "Rounding")    # simulate R 3.5
train_rf <- train(Survived ~ .,
                  data = train_set,
                  method = "rf",
                  ntree = 100,
                  tuneGrid = data.frame(mtry = seq(1:7)))
train_rf$bestTune

#What is the accuracy of the random forest model on the test set?
rf_preds <- predict(train_rf, test_set)
mean(rf_preds == test_set$Survived)


#Use varImp() on the random forest model object to determine the importance of various predictors to the random forest model.
#What is the most important variable?

varImp(train_rf)    
