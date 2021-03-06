###############################
####### CHI SQUARE DEMO #######
###############################

#Adapted from Max Mansolf's script (250A 2016)

?chisq.test

#This function perform a chi-square test
#It needs two things: observed cell counts (from data) and
#expected cell counts (from theory), in the form of probabilities.
#You don't necessarily need to pass the latter, but R will assume
#counts are then evenly distributed by category

#Make up some toy data
observed=c(1,2,3)
expected=c(2,2,2)
oMat = matrix(c(1,2,3), nrow=1, ncol=3, dimnames=list(c("counts"), c("D", "R", "I")))

#run the test
chisq.test(observed,p=expected/sum(expected))
chisq.test(observed)
#Why do you think we're getting a warning message??

#change the probabilities according to theory
expected2 = c(1,4,1)
chisq.test(observed, p = expected2/sum(expected2))

#Let's work through an (adapted) example from the documentation
M <- as.table(rbind(c(501, 314, 468, 102), c(324, 239, 477, 80)))
dimnames(M) <- list(sex = c("F", "M"),
                    fandom = c("Lakers","Clippers", "Warriors", "Kings"))
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null

