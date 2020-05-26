library(readr) #library used to read in csv files (base r has a function for this too)
library(dominanceanalysis) #package needed for dominance analysis

fyp = read_csv("Documents/Teaching/250C/Spring20/Week5/fyp.csv") #read in data (path will be different on your computer)

fypMod = lm(Stress ~ Sex + ER_Cap + ER_Tend + worMem + cogFlex, data = fyp) #run linear model, save lm object

dominanceAnalysis(fypMod) #submit lm object to dominance analysis
