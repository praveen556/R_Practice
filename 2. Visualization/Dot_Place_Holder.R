library(dslabs)
data(murders)

US_Murder_Rate <- murders %>% summarise(rate = sum(total)/sum(population)*10^5)
US_Murder_Rate
class(US_Murder_Rate)

#Turning data frame into numaric
US_Murder_Rate%>%.$rate
class(US_Murder_Rate%>%.$rate)

#Simplified code to convert to numaric

US_Murder_Rate <- murders %>% summarise(rate=sum(total)/sum(population)*10^5) %>% .$rate
class(US_Murder_Rate)

#Grouping data

heights %>% group_by(sex) %>% summarise(average=mean(height), standard_deviation=sd(height))

murders %>% group_by(region) %>% summarise(median_rate=median(total/population*10^5))

#Sorting of the data
murders <- murders %>% mutate(murder_rate=total/population*10^5)

#Sorting Ascending based on population and get the first 6 rows
murders %>% arrange(population) %>% head()

#Sorting Ascending based on murder rate and get the first 6 rows
murders %>% arrange(murder_rate) %>% head()

#Sorting descending order based on murder_rate

murders %>% arrange(desc(murder_rate)) %>% head()

#Sorting descending order and getting top 10 rows

murders %>% arrange(desc(murder_rate)) %>%top_n(10)

#Getting top 10 rows based on murder_rate
murders %>% top_n(10,murder_rate)


library(NHANES)
data(NHANES)
NHANES %>% head()
