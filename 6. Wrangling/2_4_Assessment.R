library(rvest)
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

guacamole <- list(recipe, prep_time, ingredients)
guacamole

get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
} 

get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")


library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)


nodes <- html_nodes(h, "table")

tail(nodes)

html_text(nodes[[8]])

html_table(nodes[[8]])

tab1 <- html_table(nodes[[1]])
tab1

tab2 <- html_table(nodes[[2]])
tab2

tab3 <- html_table(nodes[[3]])
tab3

tab4 <- html_table(nodes[[4]])
tab4

tab_last1 <- html_table(nodes[[21]])
tab_last1

tab_last2 <- html_table(nodes[[20]])
tab_last2

tab_last3 <- html_table(nodes[[19]])
tab_last3

tab10 <- subset(html_table(nodes[[10]], header = TRUE), select = -1)
head(tab10)

tab19 <- html_table(nodes[[19]], header = TRUE)
head(tab19)

tab10 %>% full_join(tab19, by ="Team") 



library(rvest)
library(tidyverse)
library(dplyr)
url_uk <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

h_uk <- read_html(url_uk)

nodes_uk <- html_nodes(h_uk, "table")
NROW(nodes_uk)

tab_uk_ref <- html_table(nodes_uk[[5]], fill = TRUE)
head(tab_uk_ref)
NCOL(tab_uk_ref)
