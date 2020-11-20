library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab_h <- h %>% html_node("tables")
tab_h <- tab[[2]]
tab_h


tab_h <- tab_h %>% html_table
class(tab_h)

tab <- tab_h %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

url1 <- "https://en.wikipedia.org/wiki/Composite_Index_of_National_Capability"

h1 <- read_html(url1)
class(h1)
h1

tab_h1 <- h%>% html_node("tables")
tab_h1 <- tab[[2]]
tab_h1

tab_h1 <- tab_h1 %>% html_table
class(tab_h1)

tab1 <- tab_h1 %>% setNames(c("Number","Country","CINC"))
head(tab1)
