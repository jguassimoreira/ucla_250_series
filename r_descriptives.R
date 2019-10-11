##Make the same dataframe from last time
subj = 1:8 #sub IDs
sex = c(rep(1,4), rep(0,4)) #dummy coding sex. 
sex = factor(sex, labels=c('Female', 'Male')) #Turning the sex into a factor. 
cond = factor(rep(c(rep(0,2), rep(1,2)), 2), labels = c('c', 't')) #Create a treatment and control condition
y = rnorm(8, mean = 10, sd=2) #simulate our dv; randomly draw 8 samples from a normal dist. with mean (mu) = 10, sd (sigma) = 2
d = data.frame(SubjID=subj, Sex=sex, Cond=cond, DV=y) #save these variables as columns in a dataframe & name them
d$income = rnorm(8, mean= 60000, sd = 15000) #add an additional variable (income); simulated using same procedure as above

