library(dslabs)
data("gapminder")

#Checking the total unique values in region column
length(levels(gapminder$region))

# boxplot of GDP by region in 1970

p <- gapminder %>% filter(year==1970 & !is.na(gdp)) %>% ggplot(aes(region,dollars_per_day))

p+geom_boxplot()

#Changing x axis values to horizantal

p+geom_boxplot()+
  theme(axis.text.x = element_text(angle=90, hjust=1))

#Reorder Function example
fac <- factor(c("Asia","Asia","West","West","West"))
levels(fac)

#Reordering factor with mean function
values <- c(10,8,6,4,2)
fac <- reorder(fac, values, FUN=mean)
levels(fac)

#Enhanced boxplot ordered by median income, scaled, and showing data

p <- gapminder %>% filter(year==1970 & !is.na(gdp)) %>%
  mutate(region=reorder(region, dollars_per_day, FUN=mean)) %>%
  ggplot(aes(region,dollars_per_day, fill=continent))

p+ geom_boxplot()+
  theme(axis.text.x = element_text(angle=90, hjust=1))

#Adding dots for countries and changing scale to log2

p+ geom_boxplot()+
  theme(axis.text.x = element_text(angle=90, hjust=1))+
  geom_point(show.legend = FALSE)+
  scale_y_continuous(trans = "log2")