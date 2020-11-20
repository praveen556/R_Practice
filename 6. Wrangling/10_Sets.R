# intersect vectors or data frames
intersect(1:5,4:10)
intersect(c("a","b","d"), c("b","e","f"))
tab1 <- tab[1:3,]
tab2 <- tab[2:5,]
intersect(tab1,tab2)


# perform a union of vectors or data frames

union(tab1,tab2)
union_all(tab1,tab2)

# set difference of vectors or data frames

setdiff(1:10,6:15)
setdiff(6:15, 1:10)
setdiff(tab1,tab2)
setdiff(tab2,tab1)

# setequal determines whether sets have the same elements, regardless of order

setequal(1:10,6:10)
setequal(1:5, 5:1)
setequal(tab1, tab2)
