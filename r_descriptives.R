##Make the same dataframe from last time
subj = 1:8 #sub IDs
sex = c(rep(1,4), rep(0,4)) #dummy coding sex. 
sex = factor(sex, labels=c('Female', 'Male')) #Turning the sex into a factor. 
cond = factor(rep(c(rep(0,2), rep(1,2)), 2), labels = c('c', 't')) #Create a treatment and control condition
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
