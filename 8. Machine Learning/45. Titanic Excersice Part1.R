#The Titanic was a British ocean liner that struck an iceberg and sunk on its maiden voyage in 1912 from the United Kingdom to New York. More than 1,500 of the estimated 2,224 passengers and crew died in the accident, making this one of the largest maritime disasters ever outside of war. The ship carried a wide range of passengers of all ages and both genders, from luxury travelers in first-class to immigrants in the lower classes. However, not all passengers were equally likely to survive the accident. You will use real data about a selection of 891 passengers to predict which passengers survived.
  
  #Use the titanic_train data frame from the titanic library as the starting point for this project.
  
  library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

#Split titanic_clean into test and training sets - after running the setup code, it should have 891 rows and 9 variables.

#Set the seed to 42, then use the caret package to create a 20% data partition based on the Survived column. Assign the 20% partition to test_set and the remaining 80% partition to train_set.

set.seed(42, sample.kind = "Rounding") # if using R 3.6 or later

test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE) # create a 20% test set
test_set <- titanic_clean[test_index,]
train_set <- titanic_clean[-test_index,]

#How many observations are in the training set?

nrow(train_set)

#How many observations are in the test set?

nrow(test_set)


#What proportion of individuals in the training set survived?
mean(train_set$Survived == 1)


#The simplest prediction method is randomly guessing the outcome without using additional predictors. These methods will help us determine whether our machine learning algorithm performs better than chance. How accurate are two methods of guessing Titanic passenger survival?

#Set the seed to 3. For each individual in the test set, randomly guess whether that person survived or not by sampling from the vector c(0,1) (Note: use the default argument setting of prob from the sample function).

#What is the accuracy of this guessing method?

set.seed(3, sample.kind = "Rounding")
guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
mean(guess == test_set$Survived)

#Use the training set to determine whether members of a given sex were more likely to survive or die. Apply this insight to generate survival predictions on the test set.

#What proportion of training set females survived?
train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "female") %>%
  pull(Survived)

#What proportion of training set males survived?

train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "male") %>%
  pull(Survived)


#Predict survival using sex on the test set: if the survival rate for a sex is over 0.5, predict survival for all individuals of that sex, and predict death if the survival rate for a sex is under 0.5.

#What is the accuracy of this sex-based prediction method on the test set?

sex_model <- ifelse(test_set$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
mean(sex_model == test_set$Survived) 


#In the training set, which class(es) (Pclass) were passengers more likely to survive than die?

train_set %>%
  group_by(Pclass) %>%
  summarize(Survived = mean(Survived == 1))


#Predict survival using passenger class on the test set: predict survival if the survival rate for a class is over 0.5, otherwise predict death.

#What is the accuracy of this class-based prediction method on the test set?

class_model <- ifelse(test_set$Pclass == 1, 1, 0)    # predict survival only if first class
mean(class_model == test_set$Survived)    

#Use the training set to group passengers by both sex and passenger class.

#Which sex and class combinations were more likely to survive than die?

train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Survived > 0.5)

#Predict survival using both sex and passenger class on the test set. Predict survival if the survival rate for a sex/class combination is over 0.5, otherwise predict death.

#What is the accuracy of this sex- and class-based prediction method on the test set?

sex_class_model <- ifelse(test_set$Sex == "female" & test_set$Pclass != 3, 1, 0)
mean(sex_class_model == test_set$Survived)


#Use the confusionMatrix() function to create confusion matrices for the sex model, class model, and combined sex and class model. You will need to convert predictions and survival status to factors to use this function.

#What is the "positive" class used to calculate confusion matrix metrics?

confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived))

#Which model has the highest sensitivity?
confusionMatrix(data = factor(class_model), reference = factor(test_set$Survived))

#Which model has the highest specificity?
#Which model has the highest specificity?

confusionMatrix(data = factor(sex_class_model), reference = factor(test_set$Survived))


#What is the maximum value of balanced accuracy?

##**0.806**##

#Use the F_meas() function to calculate  F1  scores for the sex model, class model, and combined sex and class model. You will need to convert predictions to factors to use this function.

#Which model has the highest  F1  score?
#What is the maximum value of the  F1  score?

F_meas(data = factor(sex_model), reference = test_set$Survived)
## [1] 0.857
F_meas(data = factor(class_model), reference = test_set$Survived)
## [1] 0.78
F_meas(data = factor(sex_class_model), reference = test_set$Survived)


