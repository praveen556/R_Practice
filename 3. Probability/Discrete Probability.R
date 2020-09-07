tab <- rep(c("Red","Blue"), times=c(2,3))
tab
#getting one sample at a time
sample(tab,1)

#replicate the size
b<-100000
samp_size<- replicate(b, sample(tab,1))
samp_size
#Getting distinct count values
tab1 <- table(samp_size)
tab1
#checking the probability of the values
prop.table(tab1)

#Using sample for multi values
tab2 <- table(sample(tab,8, replace=TRUE))

prop.table(tab2)

?set.seed
#To set the Radom code generator data to 1986-12-20
set.seed(1986)
#To reset the seed use below code
set.seed(1)
set.seed(1, sample.kind="Rounding")
