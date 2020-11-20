getwd()
#install.packages("readxl")
library("tidyverse", "dslabs") # tidyverse includes readr
library(readxl)

#inspect first 3 lines

read_lines("murders.csv", n_max = 2)

# read file in CSV format

dat <- read_csv("murders.csv")


# set path to the location for raw data files in the dslabs package and list files
path <- system.file("extdata", package = "dslabs") #To get the Path


# generate a full path to a file

filename <- "murders.csv"

fullpath <- file.path(path,filename)

#read using full path

dat <- read_csv(fullpath)
#Ex1

path <- system.file("extdata", package = "dslabs")

list.files(path)

filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"

filename2 <- "fertility-two-countries-example.csv"

file.copy(file.path(path, filename1), getwd())
file.copy(file.path(path, filename2),getwd())


dat1 <- read_csv(file.path(path, filename1))

dat2 <- read_csv(file.path(path, filename2))
