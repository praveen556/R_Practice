library(dplyr)
#Adding Rate Column to the existing Data Frame
murders <- mutate(murders,rate=total*100000/population)
murders
#Selecting Subset of columns
select(murders,state,region,rate)
#Filtering data frame
filter(murders,rate<=0.71)
#selecting subset and filtering together
select(murders,state,region,rate) %>% filter(rate<=0.71)

#Defining custom data fram

grades <- data.frame(names =c("John","Juan","Jean","Yao"),
                     exam_1=c(95,80,90,85),
                     exam_2=c(90,85,85,90),
                     stringsAsFactors = FALSE)
grades
