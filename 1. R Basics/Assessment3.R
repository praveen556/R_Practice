library(dslabs)
data(heights)
options(digits = 3)
nrow(heights)
heights <- mutate(heights, ind= height>=mean(height) )
heights
mean(heights$sex=="Female")
min(heights$height)
match(50,heights$height)
select(heights,sex,height) %>% filter(height==50.0)
max(heights$height)
mutate(heights, integer=as.integer(heights$height) )
class(heights$height)
x <- min(heights$height):max(heights$height)
sum(!x %in% heights$height)
#Adding Centimeter column
heights <- mutate(heights, heights2=height*2.54)
heights
mean(heights$heights2)

females <- select(heights, heights2) %>% filter(heights$sex=="Female")
females
class(females)
mean(females$heights2)

library(dslabs)
data("olive")
head(olive)
plot(olive$palmitic,olive$palmitoleic)
hist(olive$eicosenoic)
boxplot( palmitic~region ,data=olive)
