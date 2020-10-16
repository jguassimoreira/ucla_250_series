#Script showing how the sampling distribution, given enough repetitions of an experiment, converges on the population parameters

set.seed(88) #setting a seed for reproducibility, the number is arbitrary
s10 = replicate(10, rnorm(40, 65, 5.5)) 
hist(colMeans(s10)) #S10 is a matrix with subjects  on the rows, and samples (replications) on the columns, colMeans takes the average of each sample (replication), then we pass to hist() to plot
hist(apply(s10, 2, FUN=median)) #do the same thing, but take the median instead of mean. Apply expects a matrix, second argument is an index to applying a function (FUN) over rows (1) or columns (2)


s100 = replicate(100, rnorm(40, 65, 5.5)) #draw 40 samples from normal dist with mean of 65 and sd of 5.5; repeat this 100 times
hist(colMeans(s100))
hist(apply(s100, 2, FUN=median)) #can be any function: sd, median, min, a custom function. 


s10000 = replicate(10000, rnorm(40, 65, 5.5)) #draw 40 samples from normal dist with mean of 65 and sd of 5.5; repeat this 10000 times
sampMeans = colMeans(s10000)
hist(sampMeans)
hist(apply(s10000, 2, FUN=mean))



#see how changing the size of each sample changes the shape and range of distributions
s10.400 = replicate(10, rnorm(400, 65, 5.5))
hist(colMeans(s10.400))


s1000.400 = replicate(1000, rnorm(400, 65, 5.5))
hist(colMeans(s100.400))


