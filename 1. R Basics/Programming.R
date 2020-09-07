a <- 1
if(a!=0){print("Non Zero")
}else {print("Zero")}

library(dslabs)
data(murders)
murder_rate <- murders$total/murders$population*100000
ind <- which.min(murder_rate)
if(murder_rate[ind]<0.5){
  print(murders$state[ind])
}else{
  print("No state has Murder Rate that low")
}

a <- 1.5
ifelse(a>0,1/a,NA)

a <- c(0,2,3,4,5,100,-4)
ifelse(a>0,1/a,NA)

data(na_example)
#Replacing NA with 0 and calculating the sum
sum(ifelse(is.na(na_example),0,na_example))
z <- c(TRUE,FALSE,TRUE)
all(z)
any(z)
