#install.packages("dslabs","tidyverse")
# see working directory

getwd()

# change your working directory

setwd("C:/Users/prave/Documents/R/R_Practice/6. Wrangling/data")


# set path to the location for raw data files in the dslabs package and list files
path <- system.file("extdata", package = "dslabs") #To get the Path


list.files(path) #To list the file names

# generate a full path to a file

filename <- "murders.csv"

fullpath <- file.path(path,filename)
fullpath

# copy file from dslabs package to your working directory

file.copy(fullpath,getwd())

# check if the file exists
file.exists(filename)