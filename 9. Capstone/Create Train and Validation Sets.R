##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Quiz Movie Lens Data Set
#How many rows and columns are there in the edx dataset?

nrow(edx) #Number of Rows
ncol(edx) #Number of Columns

#How many zeros were given as ratings in the edx dataset?
nrow(edx %>% filter(rating==0))


#How many threes were given as ratings in the edx dataset?
nrow(edx %>% filter(rating==3))

#How many different movies are in the edx dataset?
nrow(distinct(edx,movieId))

#How many different users are in the edx dataset?
nrow(distinct(edx,userId))

#How many movie ratings are in each of the following genres in the edx dataset?

#Drama:
nrow(edx %>% filter(genres %like% "Drama"))

#Comedy:
nrow(edx %>% filter(genres %like% "Comedy"))

#Thriller:
nrow(edx %>% filter(genres %like% "Thriller"))

#Romance:
nrow(edx %>% filter(genres %like% "Romance"))

#Which movie has the greatest number of ratings?

tail(edx %>% group_by(title) %>% summarise(n=n()) %>% arrange(n),1)

#What are the five most given ratings in order from most to least?

head(edx %>% group_by(rating) %>% summarise(n=n()) %>% arrange(desc(n)),5)

#True or False: In general, half star ratings are less common than whole star ratings 
#(e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).


if(edx %>% filter(rating %in% c('0.5','1.5','2.5','3.5','4.5')) %>% summarise(n=n()/5)< edx %>% filter(rating %in% c('5','1','2','3','4')) %>% summarise(n=n()/5)){
  print(TRUE)
}else{
  print(FALSE)
}
