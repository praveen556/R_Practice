library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

temp_carbon
temp_carbon %>% filter(!is.na(carbon_emissions));

min<-temp_carbon %>%
  filter(!is.na(carbon_emissions))%>% select(year,carbon_emissions) %>%min()

max<-temp_carbon %>%
  filter(!is.na(carbon_emissions))%>% select(year,carbon_emissions) %>%max()
(max-min)/min


min_temp<-temp_carbon %>%
  filter(!is.na(temp_anomaly))%>% select(year,temp_anomaly) %>%min()

max_temp<-temp_carbon %>%
  filter(!is.na(temp_anomaly))%>% select(year,temp_anomaly) %>%max()
max_temp
min_temp
(max_temp-min_temp)

temp_carbon

p<- temp_carbon %>%
  filter(!is.na(temp_anomaly))%>%ggplot(aes(year,temp_anomaly))+geom_line()

p <- p + geom_hline(aes(yintercept = 0), col = "blue")
p+ylab("Temperature anomaly (degrees C)")+
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018")+
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")+
  geom_line(aes(year,ocean_anomaly, color="red"), size=1)+
  geom_line(aes(year,land_anomaly, col="green"), size=1)



greenhouse_gases %>%
  ggplot(aes(year,concentration,col=gas)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept=1850),col="red", size=2) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")


temp_carbon %>% filter(!is.na(carbon_emissions))%>%ggplot(aes(year,carbon_emissions))+geom_line()


co2_time <- historic_co2%>%ggplot(aes(year,co2, col=source))+geom_line()
co2_time+xlim(-375000,-330000)
