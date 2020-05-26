
##Sampling Distribution of betas and related content

getse <- function(linmod, x) { #defining a helper function to calculate SEs because I don't know how to pull out of lm object
  varest = sum(linmod$residuals^2) / (length(linmod$residuals) - length(linmod$coefficients))
  seb = sqrt(varest/sum((x - mean(x))^2))
}

bvec = c() #bs will go here
sevec = c() #SEs will go here


for (p in 1:5000) {
  print(sprintf("sample No. %s", p)) #Print the sample number 
  set.seed(123*p) #set seed
  x = runif(100, 1,6) #sim X
  y = rnorm(100, (1.2+ (x*.78)), 1) #sim Y based on linear relationship with X 
  bvec = c(bvec, lm(y ~ x)$coefficients[[2]]) #run regression model, save b value
  sevec = c(sevec, getse(lm(y~x), x)) #calculate SE from regression model
}

ourX = runif(100, 1,6)
ourY = rnorm(100, (1.2+ (ourX*.78)), 1)

hist(bvec, breaks = 50) #note where most of the values fall
mean(bvec)
sd(bvec) #note the standard deviation of our sampling distribution
summary(lm(ourY~ourX)) #compare that to the value of SE fron the model, our best estimate of b's sampling var
mean(sevec); median(sevec); range(sevec) #how do we do on average? Also, notice the variability in the very estimates of sampling variability 







