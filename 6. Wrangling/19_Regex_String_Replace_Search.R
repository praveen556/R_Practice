library(tidyverse)
library(dslabs)
# number of entries matching our desired pattern
pattren <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))

# inspect examples of entries with problems

problems[c(2,10,11,12,15)] %>% str_view(pattern)
str_subset(problems, "inches")
str_subset(problems, "''")

# replace or remove feet/inches words before matching

pattern <- "^[4-7]'\\d{1,2}$"

problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()


# R does not ignore whitespace
identical("Hi","Hi ")

# \\s represents whitespace

pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"

str_subset(problems, pattern_2)

# * means 0 or more instances of a character

yes <- c("ab","a1b","a11b","a111b","a111111b")
no <- c("ba","a21b")

str_detect(c(yes,no),"a1*b")


# test how *, ? and + differ
data.frame(string = c("ab","a1b","a11b","a111b","a111111b"),
           none_or_more = str_detect(yes, "a1*b"),
           nore_or_once = str_detect(yes, "a1?b"),
           once_or_more = str_detect(yes, "a1+b"))

# update pattern by adding optional spaces before and after feet symbol

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

