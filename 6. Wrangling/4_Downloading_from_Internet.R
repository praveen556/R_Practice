library(readr)
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
setwd("C:/Users/prave/Documents/R/R_Practice/6. Wrangling/data/")

dat <- read_csv(url)

download.file(url, "murders.csv")
tempfile()
tmp_filename <- tempfile()

download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)
