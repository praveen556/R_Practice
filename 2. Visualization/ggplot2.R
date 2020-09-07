library(tidyverse)
library(dslabs)
library(ggthemes) #To get themes
library(ggrepel) #To put text seperate
data(murders)

p <- ggplot(data=murders)
class(p)
p

#We can use either below syntax or ggplot(data=murders) syntax, both will be same
murders %>% ggplot()


#We added points + labels to the plot. Label should be inside the aes braces 
p <- ggplot(data=murders)
p + geom_point(aes(population/10^6,total), size=3) + 
  geom_text(aes(population/10^6,total, label=abb), nudge_x = 1.5)

#Simplyfying the code

p <- murders %>% ggplot(aes(population/10^6,total, label=abb))
p + geom_point(size=3)+
  geom_text(nudge_x = 1.5)

#local aestetic values override global values below is examples

p <- murders %>% ggplot(aes(population/10^6,total, label=abb))
p + geom_point(size=3)+
  geom_text(aes(x=100,y=200,label="Hello There"))


#Adding scale and graph Title

p <- murders %>% ggplot(aes(population/10^6,total, label=abb))
p + geom_point(size=3)+
  geom_text(nudge_x = 0.05)+
  scale_x_continuous(trans = "log10")+
  scale_y_continuous(trans = "log10")+
  xlab("Population in Millions(Log Scale)")+
  ylab("Total Number of Murders(Log Scale)")+
  ggtitle("US Gun Murders in 2010")

#Using Simplified scale functions along with colors
r <- murders %>% summarise(rate=sum(total)/sum(population)*10^6) %>% pull(rate)

p <- murders %>% ggplot(aes(population/10^6,total, label=abb))
p + geom_point(size=3)+
  geom_text(nudge_x = 0.05)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("Population in Millions(Log Scale)")+
  ylab("Total Number of Murders(Log Scale)")+
  ggtitle("US Gun Murders in 2010")+
  geom_point(aes(col=region), size=3)+
  geom_abline(intercept = log10(r))



#Using Simplified scale functions along with colors and dashed line
r <- murders %>% summarise(rate=sum(total)/sum(population)*10^6) %>% pull(rate)

p <- murders %>% ggplot(aes(population/10^6,total, label=abb))
p + geom_point(size=3)+ #size of a point
  geom_text(nudge_x = 0.05)+#space between point and text
  scale_x_log10()+ #Changing X axis to log10
  scale_y_log10()+ #Changing Y axis to log10
  xlab("Population in Millions(Log Scale)")+
  ylab("Total Number of Murders(Log Scale)")+
  ggtitle("US Gun Murders in 2010")+ 
  geom_point(aes(col=region), size=3)+ #Adding legend and default colors
  geom_abline(intercept = log10(r), lty=2, color ="dark grey")+ #Change line to dashed and color to dark grey
  geom_point(aes(col=region), size = 3)+ #Changes line to be under points
  scale_color_discrete(name = "Region")+ # Changes Legend Title
  theme_economist() #to changing the theme


#Replacing geom_text with geom_text_repel to remove the layour of texts

p <- murders %>% ggplot(aes(population/10^6,total, label=abb))
p + geom_point(size=3)+ #size of a point
  geom_text_repel()+
  scale_x_log10()+ #Changing X axis to log10
  scale_y_log10()+ #Changing Y axis to log10
  xlab("Population in Millions(Log Scale)")+
  ylab("Total Number of Murders(Log Scale)")+
  ggtitle("US Gun Murders in 2010")+ 
  geom_point(aes(col=region), size=3)+ #Adding legend and default colors
  geom_abline(intercept = log10(r), lty=2, color ="dark grey")+ #Change line to dashed and color to dark grey
  geom_point(aes(col=region), size = 3)+ #Changes line to be under points
  scale_color_discrete(name = "Region")+ # Changes Legend Title
  theme_economist() #to changing the theme