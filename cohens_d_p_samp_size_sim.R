##Helper function to calculate cohen's d for unpaired (independent samples) t-test
effectD = function(samp1, samp2) {
  
  
  #Cohen's d = group differences / standard deviation
  num = mean(samp1) - mean(samp2) #group mean differences
  denom = sqrt((var(samp1)+var(samp2))/2) #pooled SD used for denominator
  
  return(abs(num/denom))
}

effectDPop = function(m1, m2, var1, var2) {
  
  #calculate population value of cohen's d; useful for below
  
  return(abs((m1-m2)/sqrt(((var1+var2)/2))))
  
}

##Helper function that sets up our simulation
simDvP = function(simParams) {
  
  #sim params is an input vector that contains the following (in this exact order):
  #Control group n, Treatment group n, pop mean of Control group, pop mean of Treatment group, pop sd of Control group, pop sd of Treatment group, repetitions for the sim
  
  set.seed(simParams[7]) #set seed equal to some number, in this case the number of repetitions; this is arbitrary
  reps = simParams[7] #set number of replications for our simulation. A single replication can be thought of as a simulated study
  outputMat = matrix(NA, nrow = reps, ncol = 6, dimnames = list(c(), c("controlMean", "treatMean", "controlSD", "treatSD", "pVal", "d"))) #create a matrix to hold the output of each replication (simulated study)
  
  for (r in 1:reps) { #run as many simulated studies as specified by reps
    
    sampControl = rnorm(simParams[1], simParams[3], simParams[5]) #nControl, muControl, sigmaControl
    sampTreat = rnorm(simParams[2], simParams[4], simParams[6]) #nTreat, muTreat, sigmaTreat
    
    sampControlMean = mean(sampControl) #estimate sample mean of control group
    sampTreatMean = mean(sampTreat) #estimate sample mean of treatment group
    sampControlSD = sd(sampControl) #estimate sample SD of control group
    sampTreatSD = sd(sampTreat) #estimate sample SD of treatment group
    pVal = t.test(sampControl, sampTreat, paired=FALSE)$p.value #conduct a t-test, save the p-value
    d = effectD(sampControl, sampTreat) #compute the effect size
    
    outputMat[r,] = c(sampControlMean, sampTreatMean, sampControlSD, sampTreatSD, pVal, d) #save this guy to our output matrix; each row is a simulated study, each column corresponds to a quantity of interest
    
    
  }
  
  return(summary(outputMat)) #return the summary of the output matrix
  
}



#set up our simulation paraemeters
nControl = 30
nTreat = 30
muControl = 17
muTreat = 16.5
sigmaControl = 1.2
sigmaTreat = 1.2
repetitions = 1000

simParams = c(nControl, nTreat, muControl, muTreat, sigmaControl, sigmaTreat, repetitions)


simDvP(simParams) # run the simulation
effectDPop(muControl, muTreat, sigmaControl^2, sigmaTreat^2) #compare the mean of the sims' cohen's d against the population (do this at low and high sample sizes)

##You can see two things here: at low samples, cohen's d is biased (i.e., doesn't approximate population value very well); it is typically inflated
##The second thing you can see is that, as sample size increases, cohen's d stays the same, but the p-value shrinks





