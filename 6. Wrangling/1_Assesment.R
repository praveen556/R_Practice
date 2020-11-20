library("tidyverse","readr")
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
download.file(url,"wdbc.data")

read_table(url) #Just read not importing
read_csv(url) #Just read not importing
read_csv2(url) #Just read not importing
read_tsv(url) #Just read not importing

dat <- read_csv(url, col_names = FALSE)

nrow(dat)
ncol(dat)
