library(tidyverse)
library(dslabs)
data("us_contagious_diseases")
str(us_contagious_diseases)

# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting

the_disease <- "Measles"

dat <- us_contagious_diseases %>% filter(disease==the_disease & !state %in% c("Alaska", "Hawaii"))%>%
  mutate(rate = count/population*10000 * 52/weeks_reporting, state=reorder(state,rate))
dat

# plot disease rates per year in California

dat %>% filter(state=="California" & !is.na(rate))%>%
  ggplot(aes(year,rate))+
  geom_line()+
  ylab("Cases Per 10000")+
  geom_vline(xintercept = 1963, color="blue")

# tile plot of disease rate by state and year

dat %>% filter(!is.na(rate))%>% ggplot(aes(year,state, fill=rate))+geom_tile(color="grey50")+
  scale_x_continuous(expand=c(0,0))+#Resets the scake to near 0
  scale_fill_gradientn(colors = RColorBrewer :: brewer.pal(9,"Reds"), trans="sqrt")+ #Creates 2 color gradient low-high of Reds
  geom_vline(xintercept = 1963, color="blue")+ #Creating vertical Blue line to specify the start of Vaccines
  theme_minimal() + theme(panel.grid = element_blank())+
  ggtitle("The Disease")+
  xlab("")+
  ylab("")

# compute US average measles rate by year

avg <- us_contagious_diseases %>% filter(disease == the_disease) %>% group_by(year) %>%
summarize(us_rate = sum(count, na.rm = TRUE)/sum(population,na.rm = TRUE)*10000)

# make line plot of measles rate by year by state

dat %>% filter(!is.na(rate)) %>% ggplot()+
  geom_line(aes(year, rate, group=state), color="grey50", show.legend = FALSE, alpha=0.2, size=1 )+
  geom_line(mapping=aes(year,us_rate), data=avg, color="black", size=1)+ #Adding the average line to existing graph
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300))+ #changing the Y axis and breaking the scale
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("")+
  geom_text(data=data.frame(x=1955, y=50), mapping=(aes(x,y, label="US Average")), color="black")+#Adding caption of US Average
  geom_vline(xintercept = 1963, color="blue")
