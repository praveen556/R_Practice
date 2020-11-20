# load stringr through tidyverse
library(tidyverse)

# detect whether a comma is present

pattren <- ","

str_detect(murders_raw$total,pattren)

# show the subset of strings including "cm"

str_subset(reported_heights$height,"cm")

# use the "or" symbol inside a regex (|)

yes <- c("180 cm","10cm")
no <- c("10","20''", "5 Ft 10 Inches")
s <- c(yes,no)

str_detect(s,"cm")|str_detect(s,"inches")

str_detect(s,"cm|inches")

# highlight the first occurrence of a pattern

str_view(s,pattern)


# highlight all instances of a pattern
str_view_all(s, pattern)

 