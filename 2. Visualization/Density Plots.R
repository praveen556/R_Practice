library(dslabs)
data("gapminder")

#Adding dollars_per_day to the existing gapmider dataframe
gapminder <- gapminder %>% mutate(dollars_per_day=gdp/population/365)
past_year <- 1970
present_year <- 2010

# define Western countries
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")


# define countries that have data available in both years

country_list1 <- gapminder %>% filter(year==past_year & !is.na(gdp)) %>%.$country
country_list2 <- gapminder %>% filter(year==present_year & !is.na(gdp)) %>%.$country
country_list <- intersect(country_list1,country_list2)

# smooth density plots - area under each curve adds to 1 
gapminder %>% filter(year==past_year & country %in% country_list)%>%
  mutate(group=ifelse(region %in% west, "West","Developing"))%>% group_by(group)%>%
  summarise(n = n()) %>% knitr::kable()

# smooth density plots - variable counts on y-axis

p <- gapminder %>% filter(year==past_year & country %in% country_list)%>%
  mutate(group=ifelse(region %in% west, "West","Developing"))%>%
  ggplot(aes(dollars_per_day, y = ..count..,fill=group))+
  scale_x_continuous(trans = "log2")
p+geom_density(alpha=.2, bw=.75)+facet_grid(year~.)

# add group as a factor, grouping regions

gapminder <- gapminder %>% mutate(group=case_when(
  .$region %in% west ~ "West",
  .$region %in% c("Eastren Asia","South-Eastern Asia") ~ "South Asia",
  .$region %in% c("Caribbean","Centrel America","South America") ~ "Latin America",
  .$continent == "Africa" & .$region !="North Africa" ~ "Sub-Saharan Africa", TRUE ~"Others"))


# reorder factor levels
gapminder <- gapminder %>%
  mutate(group=factor(group,c("Others","Latin America","East Asia", "Sub-Saharan Africa","West")))

# note you must redefine p with the new gapminder object first
p <- gapminder %>% filter(year %in% c(past_year,present_year) & country %in% country_list)%>%
  ggplot(aes(dollars_per_day, fill=group))+
  scale_x_continuous(trans = "log2")

# stacked density plot
p+geom_density(alpha=0.2, bw=0.75, position="stack")+facet_grid(year~.)

# weighted stacked density plot
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)