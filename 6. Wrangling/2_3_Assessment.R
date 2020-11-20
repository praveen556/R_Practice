df1 <- data.frame(x = c("a","b"), y = c("a","a") )
df2 <- data.frame(x = c("a","a"), y = c("a","b") )

setdiff(df1,df2)

library(Lahman)
data(Batting)

top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

Master %>% as_tibble()

top_names <- top %>% left_join(Master)%>%
  select(playerID, nameFirst, nameLast, HR)
top_names

head(Salaries)

top_salary <- Salaries %>% filter(yearID == 2016) %>% right_join(top_names)  %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

head(AwardsPlayers)

Awards_2016 <- AwardsPlayers %>% filter(yearID==2016) %>% select(playerID)
Awards_2016

top_awards <- Awards_2016 %>% semi_join(top_names)

distinct(top_awards)  %>% count()

distinct(Awards_2016 %>% anti_join(top_names)) %>% count()

      