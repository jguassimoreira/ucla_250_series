###############################
#      CORRELATIONS IN R      #
###############################

#Let's go over how to run correlations in R
data(cars) #Speeds and stopping distances of cars from the 1920s
data(iris) #classic dataset measuring physical characteristics of different species of iris plants

?cor #family of functions that gives us variances, covariances, and correlations
#type in just the name of the function (e.g,. var) to see its sources clode!
cov(cars$speed, cars$dist)
cov(iris$Petal.Length, iris$Petal.Width)
cov(iris$Sepal.Length, iris$Sepal.Width)

#Is it easy to compare between those covariances? 
#NO! They're not standardized
#Let's create a table below to help better illustrate

matrix(c(cov(cars$speed, cars$dist), cov(iris$Petal.Length, iris$Petal.Width), cov(iris$Sepal.Length, iris$Sepal.Width),
         cor(cars$speed, cars$dist), cor(iris$Petal.Length, iris$Petal.Width), cor(iris$Sepal.Length, iris$Sepal.Width)), 2,3, byrow = TRUE,
        dimnames=list(c("Covariances", "Correlations"), c("Speed X Dist", "Petal Length X Petal Width", 'Sepal Length X Septal Width')))

#Let's inspect correlation a little more closely
cor(cars$speed, cars$dist)

#Gives us a correlation value, yay!
#But nothing else. What would we use? 

?cor.test

#Before running any tests, we should probably visualize the relationship between the variables
plot(cars$speed, cars$dist)

#Let's use this function to get some more info (CIs, p values etc.)
cor.test(cars$speed, cars$dist)

#What if we wanted a 99% CI? 
cor.test(cars$speed, cars$dist, conf.level = 0.99)

#What if we didn't want to assume a linear relationship between them? 
cor.test(cars$speed, cars$dist, method = "spearman")


#Tedious to run pairwise correlations for more than two variables. Let's use the psych package to submit a dataframe and get back a correlation matrix
require(psych)

corr.test(iris[,c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width")])



###############################
#         ANOVA IN R          #
###############################

#Let's sim some data to get this done!
#Let's suppose we surveyed fans of 4 CA baseball teams to see how much they spent, on average, at a ballgame
#We want to see how thrifty (or not) each fanbase is!

#Sim some data from four different groups (different means, same sigma)
set.seed(44)
LAD = rnorm(15, 11.07, 2.75) #Dodgers
LAA = rnorm(15, 15.40, 2.75) #Angels
SDP = rnorm(15, 18.09, 2.75) #Padres
SFG = rnorm(15, 19.50, 2.75) #Giants

#concatenate our data into one vector
DV_money_spent <- c(LAD, LAA, SDP, SFG)

#Create our group (IV) labels
IV_fan_affil <- c(rep("LAD", 15), rep("LAA", 15), rep("SDP", 15), rep("SFG", 15))

#run the model, save it as an object 
#(This procedure is going to become pretty common the more modeling you do in R)
?aov
mod <- aov(DV_money_spent~IV_fan_affil)
mod #There's useful info here, but it's not really a convenient report for our purposes

summary(mod)


#Sim some data from four different groups (different means, same sigma) with very small sample sizes
set.seed(44)
LAD = rnorm(5, 11.07, 2.75) #Dodgers
LAA = rnorm(5, 15.40, 2.75) #Angels
SDP = rnorm(5, 18.09, 2.75) #Padres
SFG = rnorm(5, 19.50, 2.75) #Giants

#concatenate our data into one vector
DV_money_spent <- c(LAD, LAA, SDP, SFG)

#Create our group (IV) labels
IV_fan_affil <- c(rep("LAD", 5), rep("LAA", 5), rep("SDP", 5), rep("SFG", 5))

#run the model, save it as an object 
#(This procedure is going to become pretty common the more modeling you do in R)

mod <- aov(DV_money_spent~IV_fan_affil)


summary(mod)

