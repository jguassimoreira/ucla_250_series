#### Quick basics in R ####
##This is a script; a document saved with instructions/commands for R to execute. 

#### Syntax Basics ####

x = 2 #tell computer to store number 2 in memory, label as x
x #this will print the value of 2 for us

#don't have to save into a variable and then print it.
1:40 #this will just print 1 through 40 on the screen
#btw, R will see the colon above and fill in the integers between

#### Data Types ####


##Numeric - default class for all things we want to treat as numbers
class(5) #check if the input (the 5) is a number
is.numeric(-10.4) #will return TRUE if numeric

#this may seem trivial now, but these functions can be useful if pre-processing
#data. E.g., Tell you script to do one thing if a datum is numeric, check to 
#make sure your script does what it's supposed to, etc. 


##Characters - text strings
class("hello")
class("1") #this is different than before, because now we've put "" around a number

#strings can be pasted together with the paste() function
paste("hello", "there", sep=" ")
#remove the space to see what the sep argument does
paste("hello", "there", sep="")
paste("See", "ALL", "the", "strings!", sep=" ") #can pass more than 2 strings

#Strings can be indexed with the substr function
substr("Hello World!", 3, 10) #print 3rd to 10th character
#You can also pass a variable that points to a string to this function 
#(actually, the same principle applies for other functions)
#e.g., 
s = "Silly Sally"
substr(s, 2, 7)


##Vectors
#1 dimensional matrix (a row of data); must all be of the same type
c(1,4,15,62,3.5,-78) #print vector of numbers
c("This", "is", "a", "string", "vector") #princt vector of strings
c(1, "2") #see what happens when you try to give two different classes to a vector

#vectors are indexed with square brackets
y = c(1,4,9,16,25)
y[3] #gives us 3rd element (9)
y[-1] #give us everything but first element
y[2:4] # give us 2nd through 4th element -- indexing starts at 1, not 0!


##Matrices
#2 dimensional array of data. Must all be of the same tpe
#create by giving data to matrix() function and specifying dimensions
mat = matrix(1:12,nrow=3)

#use byrow=TRUE argument to input data by row instead of column
mat = matrix(1:12, nrow=3, byrow=TRUE)

#Store data from the vector 1:12 into a matrix with 3 rows; data will be added by column (default); give row and column names
mat = matrix(1:12,nrow=3, dimnames=list(c("R1", "R2", "R3"), c("C1", "C2", "C3", "C4")))

#matrices are indexed with two coordinates in square brackets
#Can return one datum, or a subset of data
mat[2,3] #gives us datum at 2nd row, 3rd column
mat[1,] #returns 1st row
mat[,2] #returns second column (prints horizontally)
mat[1,2:3] #values of the 2nd - 3rd column in 1st row


##Data Frames
#Store columns of different classes! (among other useful features)
subj = 1:8 #sub IDs
sex = c(rep(1,4), rep(0,4)) #dummy coding sex. We'll talk more about what dummy coding means later on
sex = factor(sex, labels=c('Female', 'Male')) #Turning the sex into a factor. This is helpful for later statistical modeling
#visit here for more on factors: https://www.tutorialspoint.com/r/r_factors.htm
cond = factor(rep(c(rep(0,2), rep(1,2)), 2), labels = c('c', 't')) #Create a treatment and control condition; lowest integergets the first label etc
y = rnorm(8, mean = 10, sd=2) #simulate our dv; randomly draw 8 samples from a normal dist. with mean (mu) = 10, sd (sigma) = 2
d = data.frame(SubjID=subj, Sex=sex, Cond=cond, DV=y) #save these variables as columns in a dataframe & name them
d$income = rnorm(8, mean= 60000, sd = 15000) #add an additional variable (income); simulated using same procedure as above
d = d[,1:4] #removes income variable we just added; could also do this with: d = d[,-5]