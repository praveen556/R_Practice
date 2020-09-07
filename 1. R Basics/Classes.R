#importing dslab library
library(dslabs)

#Reading data set Murders
data(murders)

#Verifying the type of the object
class(murders)

#Checking the structure of an object
str(murders)

#Checking the data in one of the column Population from the murders data set
murders$population

#Displaying the column names from the Murders dataset
names(murders)

#Checking the number of rows in a data frame after assiging it to a variable
pop <- murders$population
length(pop)

#Checking the clasification of a different columns
class(pop)
class(murders$state)

#Testing logical vectors
abc <- 556==553
class(abc)

#Checking classification of factors, factors are kind of a subgroups
class(murders$region)

#Obtaining levels of Factors, this will give unique values from the factor
levels(murders$region)