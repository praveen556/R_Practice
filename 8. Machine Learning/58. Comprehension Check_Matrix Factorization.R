#In this exercise set, we will be covering a topic useful for understanding matrix factorization: the singular value decomposition (SVD). SVD is a mathematical result that is widely used in machine learning, both in practice and to understand the mathematical properties of some algorithms. This is a rather advanced topic and to complete this exercise set you will have to be familiar with linear algebra concepts such as matrix multiplication, orthogonal matrices, and diagonal matrices.

#The SVD tells us that we can decompose an  N×p  matrix  Y  with  p<N  as 

#Y=UDV⊤ 
#with  U  and  V  orthogonal of dimensions  N×p  and  p×p  respectively and  D  a  p×p  diagonal matrix with the values of the diagonal decreasing: 
  
#  d1,1≥d2,2≥…dp,p 
#In this exercise, we will see one of the ways that this decomposition can be useful. To do this, we will construct a dataset that represents grade scores for 100 students in 24 different subjects. The overall average has been removed so this data represents the percentage point each student received above or below the average test score. So a 0 represents an average grade (C), a 25 is a high grade (A+), and a -25 represents a low grade (F). You can simulate the data like this:

set.seed(1987, sample.kind="Rounding")
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))


#Our goal is to describe the student performances as succinctly as possible. For example, we want to know if these test results are all just a random independent numbers. Are all students just about as good? Does being good in one subject  imply you will be good in another? How does the SVD help with all this? We will go step by step to show that with just three relatively small pairs of vectors we can explain much of the variability in this  100×24  dataset. 

#You can visualize the 24 test scores for the 100 students by plotting an image:

my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

#Which of the following best describes what you see?


##**The students that test well are at the top of the image and there seem to be three groupings by subject.**##

#You can examine the correlation between the test scores directly like this:


my_image(cor(y), zlim = c(-1,1))
range(cor(y))

axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Which of the following best describes what you see?

##**There is correlation among all tests, but higher if the tests are in science and math and even higher within each subject.**##

#Remember that orthogonality means that  U⊤U  and  V⊤V  are equal to the identity matrix. This implies that we can also rewrite the decomposition as

#YV=UD or U⊤Y=DV⊤ 
#We can think of  YV  and  U⊤V  as two transformations of  Y  that preserve the total variability of  Y  since  U  and  V  are orthogonal.

#Use the function svd() to compute the SVD of y. This function will return  U ,  V , and the diagonal entries of  D .


s <- svd(y)
names(s)

#You can check that the SVD works by typing:

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))


#Compute the sum of squares of the columns of  Y  and store them in ss_y. Then compute the sum of squares of columns of the transformed  YV  and store them in ss_yv. Confirm that sum(ss_y) is equal to sum(ss_yv).

#What is the value of sum(ss_y) (and also the value of sum(ss_yv))?

ss_y <- apply(y^2, 2, sum)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_y)
sum(ss_yv)

#We see that the total sum of squares is preserved. This is because  V  is orthogonal. Now to start understanding how  YV  is useful, plot ss_y against the column number and then do the same for ss_yv.

#What do you observe?
plot(ss_y)
plot(ss_yv)


#Now notice that we didn't have to compute ss_yv because we already have the answer. How? Remember that  YV=UD  and because  U  is orthogonal, we know that the sum of squares of the columns of  UD  are the diagonal entries of  D  squared. Confirm this by plotting the square root of ss_yv versus the diagonal entries of  D .

#Which of these plots is correct?

data.frame(x = sqrt(ss_yv), y = s$d) %>%
  ggplot(aes(x,y)) +
  geom_point()


#So from the above we know that the sum of squares of the columns of  Y  (the total sum of squares) adds up to the sum of s$d^2 and that the transformation  YV  gives us columns with sums of squares equal to s$d^2. Now compute the percent of the total variability that is explained by just the first three columns of  YV .

#What proportion of the total variability is explained by the first three columns of  YV ?

sum(s$d[1:3]^2) / sum(s$d^2)


#Before we continue, let's show a useful computational trick to avoid creating the matrix diag(s$d). To motivate this, we note that if we write  U  out in its columns  [U1,U2,…,Up]  then  UD  is equal to

#UD=[U1d1,1,U2d2,2,…,Updp,p] 
#Use the sweep function to compute  UD  without constructing diag(s$d) or using matrix multiplication.

#Which code is correct?

identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))


#We know that  U1d1,1 , the first column of  UD , has the most variability of all the columns of  UD . Earlier we looked at an image of  Y  using my_image(y), in which we saw that the student to student variability is quite large and that students that are good in one subject tend to be good in all. This implies that the average (across all subjects) for each student should explain a lot of the variability. Compute the average score for each student, plot it against  U1d1,1 , and describe what you find.

#What do you observe?

plot(s$u[,1]*s$d[1], rowMeans(y))

#We note that the signs in SVD are arbitrary because:

#UDV⊤=(−U)D(−V)⊤ 
#With this in mind we see that the first column of  UD  is almost identical to the average score for each student except for the sign.

#This implies that multiplying  Y  by the first column of  V  must be performing a similar operation to taking the average. Make an image plot of  V  and describe the first column relative to others and how this relates to taking an average.

#How does the first column relate to the others, and how does this relate to taking an average?

my_image(s$v)


#The following four exercises are all ungraded and are provided to give you an additional opportunity to practice working with matrices in a continuation of the exercises with this dataset.

#We recommend that you attempt to write the code on your own before hitting "submit" and viewing the answers.

#We already saw that we can rewrite  UD  as

#U1d1,1+U2d2,2+⋯+Updp,p 
#with  Uj  the j-th column of  U . This implies that we can rewrite the entire SVD as:
  
#  Y=U1d1,1V⊤1+U2d2,2V⊤2+⋯+Updp,pV⊤p 
#with  Vj  the jth column of  V . Plot  U1 , then plot  V⊤1  using the same range for the y-axis limits, then make an image of  U1d1,1V⊤1  and compare it to the image of  Y . Hint: use the my_image() function defined above. Use the drop=FALSE argument to assure the subsets of matrices are matrices.
plot(s$u[,1], ylim = c(-0.25, 0.25))

with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))

my_image(y)

#We see that with just a vector of length 100, a scalar, and a vector of length 24, we can actually come close to reconstructing the a  100×24  matrix. This is our first matrix factorization:

#Y≈d1,1U1V⊤1 
#In the exercise in Q6, we saw how to calculate the percent of total variability explained. However, our approximation only explains the observation that good students tend to be good in all subjects. Another aspect of the original data that our approximation does not explain was the higher similarity we observed within subjects. We can see this by computing the difference between our approximation and original data and then computing the correlations. You can see this by running this code:
  

resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Now that we have removed the overall student effect, the correlation plot reveals that we have not yet explained the within subject correlation nor the fact that math and science are closer to each other than to the arts. So let's explore the second column of the SVD.

#Repeat the previous exercise (Q10) but for the second column: Plot  U2 , then plot  V⊤2  using the same range for the y-axis limits, then make an image of  U2d2,2V⊤2  and compare it to the image of resid.

plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))

with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))

my_image(resid)


#The third column clearly relates to a student's difference in ability in math and science. We can see this most clearly from the plot of s$v[,3]. Adding the matrix we obtain with these two columns will help with our approximation:

#Y≈d1,1U1V⊤1+d2,2U2V⊤2+d3,3U3V⊤3 
#We know it will explain: sum(s$d[1:3]^2)/sum(s$d^2) * 100 percent of the total variability. We can compute new residuals like this:


resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)


#We no longer see structure in the residuals: they seem to be independent of each other. This implies that we can describe the data with the following model:

#Y=d1,1U1V⊤1+d2,2U2V⊤2+d3,3U3V⊤3+ε 
#with  ε  a matrix of independent identically distributed errors. This model is useful because we summarize of  100×24  observations with  3×(100+24+1)=375  numbers.

#Furthermore, the three components of the model have useful interpretations:
  
#  1 - the overall ability of a student

#2 - the difference in ability between the math/sciences and arts

#3 - the remaining differences between the three subjects.

#The sizes  d1,1,d2,2  and  d3,3  tell us the variability explained by each component. Finally, note that the components  dj,jUjV⊤j  are equivalent to the jth principal component.

#Finish the exercise by plotting an image of  Y , an image of  d1,1U1V⊤1+d2,2U2V⊤2+d3,3U3V⊤3  and an image of the residuals, all with the same zlim.

plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))

with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))

my_image(resid)  
