staff <- c("Mandy, Chris and Laura","Steve, Ruth and Frank")
day <- c("Monday","Tuesday")

schedule <- data.frame(day,staff)

str_split(schedule$staff, ", | and ")

str_split(schedule$staff, ",\\s|\\sand\\s")

tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest(cols = c(staff))


dat <- gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(country_short = recode(country, 
                                "Central African Republic" = "CAR", 
                                "Congo, Dem. Rep." = "DRC",
                                "Equatorial Guinea" = "Eq. Guinea"))
dat

library(rvest)
library(tidyverse)
library(stringr)
library(dplyr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

names(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
polls <- polls[str_detect(polls$remain, "%"), -9]
nrow(polls)


as.numeric(str_replace(polls$remain, "%", ""))/100
parse_number(polls$remain)/100

str_replace(polls$undecided, "N/A","0")


temp <- str_extract_all(polls$dates, "\\d{1,2}\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
temp
end_date

temp <- str_extract_all(polls$dates, "[0-9]+\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
temp
end_date


temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
temp
end_date


temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]{3,5}")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
temp
end_date