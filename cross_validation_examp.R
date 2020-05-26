
##Sampling Distribution of betas and related content

getse <- function(linmod, x) { #defining a helper function to calculate SEs because I don't know how to pull out of lm object
  varest = sum(linmod$residuals^2) / (length(linmod$residuals) - length(linmod$coefficients))
  seb = sqrt(varest/sum((x - mean(x))^2))
}

bvec = c() #bs will go here
sevec = c() #SEs will go here


for (p in 1:10000) {
  print(sprintf("sample No. %s", p)) #Print the sample number 
  set.seed(123*p) #set seed
  x = rnorm(100, 0, .876) #sim X
  y = rnorm(100, x*.78, .15) #sim Y based on linear relationship with X (no intercept to make things easy)
  bvec = c(bvec, lm(y ~ x)$coefficients[[2]]) #run regression model, save b value
  sevec = c(sevec, getse(lm(y~x), x)) #calculate SE from regression model
}

hist(bvec) #note where most of the values fall
mean(bvec) #equals our sample value
sd(bvec) #note the standard deviation of our sampling distribution
summary(lm(y~x)) #compare that to the value of SE fron the model, our best estimate of b's sampling var
mean(sevec); median(sevec); range(sevec) #how do we do on average? Also, notice the variability in the very estimates of sampling variability 

##R square v adjusted R square example

set.seed(123) #set seed
x1 = runif(100000, 1, 5) #sim X1
x2 = runif(100000, 1, 5)
x3 = runif(100000, 1, 5)
#y = rnorm(100, x*.41, .75) #sim 
y = 1.2 + (.47*x1) + (.2*x2) + (.731*x3)  + rnorm(100000, 0, 1)

mod = lm(y~x1+x2+x3)
summary(mod)

summary(mod)$r.squared #pull out r squared
summary(mod)$adj.r.squared  # if you wanted to pull out adjusted r squared


r2_50 = c()
adjr2_50 = c()
for (i in 1:5000) {
  
  set.seed(i*i)
  xi1 = runif(50, 1, 5) #sim X1
  xi2 = runif(50, 1, 5)
  xi3 = runif(50, 1, 5)
  #y = rnorm(100, x*.41, .75) #sim 
  yi = 1.2 + (.47*xi1) + (.2*xi2) + (.731*xi3)  + rnorm(50, 0, 1)
  
  r2_50 = c(r2_50,summary(lm(yi~xi1+xi2+xi3))$r.squared)
  adjr2_50 = c(adjr2_50,summary(lm(yi~xi1+xi2+xi3))$adj.r.squared)
  
}


r2_125 = c()
adjr2_125 = c()
for (i in 1:5000) {
  
  set.seed(i*i)
  xi1 = runif(125,1, 5) #sim X1
  xi2 = runif(125,1, 5)
  xi3 = runif(125,1, 5)
  #y = rnorm(100, x*.41, .75) #sim 
  yi = 1.2 + (.47*xi1) + (.2*xi2) + (.731*xi3)  + rnorm(125, 0, 1)
  
  r2_125 = c(r2_125,summary(lm(yi~xi1+xi2+xi3))$r.squared)
  adjr2_125 = c(adjr2_125,summary(lm(yi~xi1+xi2+xi3))$adj.r.squared)
  
}


r2_200 = c()
adjr2_200 = c()
for (i in 1:5000) {
  
  set.seed(i*i)
  xi1 = runif(200, 1, 5) #sim X1
  xi2 = runif(200, 1, 5)
  xi3 = runif(200, 1, 5)
  #y = rnorm(100, x*.41, .75) #sim 
  yi = 1.2 + (.47*xi1) + (.2*xi2) + (.731*xi3)  + rnorm(200, 0, 1)
  
  r2_200 = c(r2_200,summary(lm(yi~xi1+xi2+xi3))$r.squared)
  adjr2_200 = c(adjr2_200,summary(lm(yi~xi1+xi2+xi3))$adj.r.squared)
  
}

r2_500 = c()
adjr2_500 = c()
for (i in 1:5000) {
  
  set.seed(i*i)
  xi1 = runif(500, 1, 5) #sim X1
  xi2 = runif(500, 1, 5)
  xi3 = runif(500, 1, 5)
  #y = rnorm(100, x*.41, .75) #sim 
  yi = 1.2 + (.47*xi1) + (.2*xi2) + (.731*xi3)  + rnorm(500, 0, 1)
  
  r2_500 = c(r2_500,summary(lm(yi~xi1+xi2+xi3))$r.squared)
  adjr2_500 = c(adjr2_500,summary(lm(yi~xi1+xi2+xi3))$adj.r.squared)
  
}























