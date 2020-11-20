
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

# filename is defined in the previous video
# read.csv converts strings to factors
dat2 <- read.csv(filename)
class(dat2$abb)
class(dat2$region)