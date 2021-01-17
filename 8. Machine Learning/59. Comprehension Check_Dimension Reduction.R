#We want to explore the tissue_gene_expression predictors by plotting them.


data("tissue_gene_expression")
dim(tissue_gene_expression$x)

#We want to get an idea of which observations are close to each other, but, as you can see from the dimensions, the predictors are 500-dimensional, making plotting difficult. Plot the first two principal components with color representing tissue type.

#Which tissue is in a cluster by itself?

pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

##**liver**##

#The predictors for each observation are measured using the same device and experimental procedure. This introduces biases that can affect all the predictors from one observation. For each observation, compute the average across all predictors, and then plot this against the first PC with color representing tissue. Report the correlation.

#What is the correlation?

avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()

cor(avgs, pc$x[,1])


#We see an association with the first PC and the observation averages. Redo the PCA but only after removing the center. Part of the code is provided for you.

x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x))) #BLANK
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()


#For the first 10 PCs, make a boxplot showing the values for each tissue.

#For the 7th PC, which two tissues have the greatest median difference?

for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}


#Plot the percent variance explained by PC number. Hint: use the summary function.

#How many PCs are required to reach a cumulative percent variance explained greater than 50%?

plot(summary(pc)$importance[3,])
