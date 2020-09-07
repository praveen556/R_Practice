options(digits = 3)
library(tidyverse)
library(titanic)
data("titanic_train")
titanic_train

titanic <- titanic_train %>% select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived=factor(Survived), Pclass=factor(Pclass), Sex=factor(Sex))

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age, color=Sex))+ geom_density()

titanic %>% group_by(Sex) %>% summarise(total=sum(ifelse(Sex=="Male",1,1))) 

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

index <- !is.na(titanic$Age)

x <- titanic$Age[index]
x
z <- scale(x)
z

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

#calculate observed and theretical quantile
p <- seq(0.01,0.99,0.01)
observed_quantiles <- quantile(x,p)
theoritical_quantile <- qnorm(p,mean = mean(x), sd = sd(x))
#make QQ Plot
plot(theoritical_quantile,observed_quantiles)
abline(0,1)
geom_qq( dparams =  params)

#Bar Plot
titanic %>% ggplot()+geom_bar(aes(Survived, fill=Sex))

#Density Plot

titanic %>%filter(!is.na(Age))%>% 
  mutate(Age_group= case_when(Age<=8 ~"0-8",
                              Age>9 & Age<=18 ~"10-18",
                              Age>18 & Age<=30 ~"18-30",
                              Age>30 & Age<=50 ~"30-50",
                              Age>50 & Age<=70 ~"50-70",
                              Age>70 & Age<=80 ~"70-80"))%>% 
  ggplot(aes(Survived,y = ..count.., fill=Age_group))+geom_density(alpha=0.2)

p <- titanic %>% filter(!is.na(Age)) %>%
  mutate(Age_group= case_when(Age<=8 ~"0-8",
                              Age>9 & Age<=18 ~"10-18",
                              Age>18 & Age<=30 ~"18-30",
                              Age>30 & Age<=50 ~"30-50",
                              Age>50 & Age<=70 ~"50-70",
                              Age>70 & Age<=80 ~"70-80"))%>%
  group_by(Survived,Age_group)%>% summarise(total=sum(ifelse(Sex=="male",1,1)))


p%>%select(Age_group,Survived,total) %>%
  spread(Survived,total)%>%mutate(per=`0`*100/(`0`+`1`))

#Survival by Fare

titanic %>% filter(Fare>0) %>%ggplot(aes(Survived, Fare),trans="log2")+geom_boxplot()+
  geom_point(alpha=0.2)+geom_jitter()


#BarPlots


titanic %>% ggplot()+geom_bar(aes(Pclass,fill=Survived))

titanic %>% ggplot()+geom_bar(aes(Pclass,fill=Survived),position=position_fill())

titanic %>% ggplot()+geom_bar(aes(Survived,fill=Pclass),position=position_fill())


#Denisity Plot

titanic %>% ggplot(aes(Age,y = ..count.., fill=Survived))+geom_density()+facet_wrap(paste0(Sex,Pclass)~.)
