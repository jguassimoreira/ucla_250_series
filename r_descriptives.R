##Make the same dataframe from last time
subj = 1:8 #sub IDs
sex = c(rep(1,4), rep(0,4)) #dummy coding sex. 
sex = factor(sex, labels=c('Female', 'Male')) #Turning the sex into a factor. 
cond = factor(rep(c(rep(0,2), rep(1,2)), 2), labels = c('c', 't')) #Create a treatment and control condition
set.seed(13)
y = rnorm(8, mean = 10, sd=2) #simulate our dv; randomly draw 8 samples from a normal dist. with mean (mu) = 10, sd (sigma) = 2
d = data.frame(SubjID=subj, Sex=sex, Cond=cond, DV=y) #save these variables as columns in a dataframe & name them
d$income = rnorm(8, mean= 60000, sd = 15000) #add an additional variable (income); simulated using same procedure as above


##Attaching a dataframe
attach(d) #you now do not need to use '$' every time to call a variable within d
DV #See, no '$'
#NOTE: if you attach multiple objects with the same name, they will be masked
#This can cause problems later on. So either don't attach your data, or detach when finished
detach(d)

##Descriptive statistics (https://www.statmethods.net/stats/descriptives.html)
mean(d$DV) #mean of a dataframe variable

mean(y) #mean of a vector

mean(matrix(rnorm(100), nrow=10)) #mean of a matrix

summary(d) #calculating summary statistics for the entire dataframe

summary(d$income) #calculating summary stats for a single variable within the dataframe

var(y) #calculating the variance

sd(d$income) #calculate the standard deviation
#Side note: Read the documentation!!
?sd #some non-statistically inclined softwares will use denominator of n (e.g., excel has two different functions, numpy documentation is opaque)

#What if you wanted just n on the denominator? (i.e., you have everyone in your population)

popVar = function(x) {
  
  #A function that returns the population variance (i.e., n, not n-1, on the denominator)
  smsq = var(y) * (length(y) - 1)  
  pVar = smsq / length(y)
  
  return(pVar)
  
}

sd(matrix(rnorm(100), nrow=10)) #sd of all elements in a matrix

sd(d) #won't work since some variables aren't numeric

median(d$income)

range(d$income) #returns vector of lowest and highest val

range(d$income)[1] #get lowest value, use [2] for highest value
#min() and max() will give you these two values as well, respectively

#Let's calculate cell and marginal means with aggregate
aggregate(d[,c("DV", "income")], by = list(d$Sex), FUN=mean)

aggregate(d[,c("DV", "income")], by = list(d$Sex, d$Cond), FUN=mean)
