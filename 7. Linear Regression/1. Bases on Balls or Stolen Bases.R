library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

#Scatterplot of the relationship between HRs and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_Per_Game = HR/G, R_Per_Game = R/G) %>%
  ggplot(aes(HR_Per_Game, R_Per_Game))+
  geom_point(alpha=0.5)

#Scatterplot of the relationship between stolen bases and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_Per_Game = SB/G, R_Per_Game = R/G) %>%
  ggplot(aes(SB_Per_Game, R_Per_Game))+
  geom_point(alpha =0.5)

#Scatterplot of the relationship between bases on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_Per_Game = BB/G, R_Per_Game=R/G)%>%
  ggplot(aes(BB_Per_Game, R_Per_Game))+
  geom_point(alpha=0.5)

#Scatterplot of runs per game versus at bats (AB) per game.
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(R_Per_Game = R/G, AB_Per_Game= AB/G) %>%
  ggplot(aes(R_Per_Game, AB_Per_Game))+
  geom_point(alpha=0.5)


#Scatterplot of win rate (number of wins per game) versus number of fielding errors (E) per game.
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_Per_Game = W/G, E_Per_Game= E/G) %>%
  ggplot(aes(W_Per_Game, E_Per_Game))+
  geom_point(alpha=0.5)


#Scatterplot of triples (X3B) per game versus doubles (X2B) per game.
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_Per_Game = X3B/G, X2B_Per_Game= X2B/G) %>%
  ggplot(aes(X3B_Per_Game, X2B_Per_Game))+
  geom_point(alpha=0.5)
