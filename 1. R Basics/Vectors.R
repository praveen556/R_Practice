#Defining Vectors
codes <- c(380,124,818)
country <- c("Italy","Canada","Egypt")

#Defining names to a vector
codes <- c("Italy"=380, "Canada"=124, "Egypt"=818)
codes
codes <- c(italy=380, canada=124, egypt=819)
codes

#Accessing specific part of vector
codes[1]
codes[c(2,3)]
codes[1:3]

#Accessing based on names
codes["italy"]
codes[c("italy","egypt")]