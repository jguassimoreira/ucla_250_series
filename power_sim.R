######################################
########## POWER ANALYSIS ############
######################################

#Adapted from Max Mansolf's script (250A 2016)

########################################################
# INGREDIENTS FOR A POWER ANALYSIS - ONE-SAMPLE Z TEST #
# We're going to do a power analysis for a one-tailed, one-sample z test,
# NULL HYPOTHESIS: mean IQ score for UCLA psychology graduate students = 100
# ALTERNATIVE HYPOTHESIS: mean IQ scores for UCLA psychology graduate students = 110
# alpha = .05

#null population mean - average IQ score is 100
muH0=100
#alternative population mean - average IQ score is 110
muH1=110
#population SD - 10
sigmaPop=10
#critical value for one-tailed test - from qnorm()
zCrit=qnorm(.95, 0, 1) #a z statistic beyond this value will lead to rejecting the null
#Check with pnorm
pnorm(1.644854, 0, 1) #these latter two arguments aren't strictly necessary, but I'm including them here for purposes of transparency/readability

#sample size = 38, the size of our class! (I think)
n=38

#standard error
sePop=sigmaPop/sqrt(n)
sePop

#############################
# STEPS OF A POWER ANALYSIS #

#1. Find raw score at which the null will be rejected; raw critical value
rawCrit=muH0 + sePop*zCrit

#2. Convert this raw score to a z statistic under the alternative hypothesis
zAlt=(rawCrit - muH1) / sePop
zAlt

#3. Determine probability of obtaining a z-score greater than z.alternative
1-pnorm(zAlt) #very high power!

#######################################
# ALTERNATIVE PROCEDURE - RESAMPLING! #
#sample lots of means (use SE instead of SD) from the alternative distribution
altSamp=rnorm(100000,mean = muH1,sd = sePop)

#p-values, under the null hypothesis, for these points
pNull=1-pnorm(altSamp,mean = muH0,sd = sePop)

#calculate number which would be rejected under the null
numReject=sum(pNull<.05)
numReject/100000 #empirical power estimate
1-pnorm(zAlt) #theoretical power estimate

###########################################
# RESAMPLE SAMPLES AND CALCULATE THE MEAN #
#sample lots of means
altResamp = rep(NA,100000)
for(i in 1:100000){
  muthisi=rnorm(n,mean = muH1,sd = sePop)
  altResamp[i]=mean(muthisi)
}

#p-values, under the null hypothesis, for these points
pNullResamp=1-pnorm(altResamp,mean = muH0,sd = sePop)

#calculate number which would be rejected under the null
numRejectResamp=sum(pNullResamp<.05)
numRejectResamp/100000 #empirical power estimate
1-pnorm(zAlt) #theoretical power estimate



##Note that you can use this basic framework to figure out power for a number of statistical tests. You could use complicated/proprietary software
#Or you could write a simulation like this one and check, empirically, what power should be given the assumed effects/parameters of your study
#more complicated studies will require more complicated sims, but the general conceptual framework presented here holds 