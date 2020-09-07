library(dslabs)
data(heights)
head(heights)

x <- heights %>% filter(sex=="Male") %>% pull(height) 

#Male height below 70 inches

male_height <- function(a)mean(x<=a)
male_height(70) #Male height below 70 inches
1-male_height(70)#Male height above 70 inches

