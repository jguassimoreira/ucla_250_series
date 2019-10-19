##Helper function to calculate cohen's d for unpaired (independent samples) t-test
effectD = function(samp1, samp2) {
  
  
  #Cohen's d = group differences / standard deviation
  num = mean(samp1) - mean(samp2) #group mean differences
  denom = sqrt((var(samp1)+var(samp2))/2) #pooled SD used for denominator
  
  return(abs(num/denom))
}

effectDPop = function(m1, m2, var1, var2) {
  
  #calculate population value of cohen'sd
  
  return(abs((m1-m2)/sqrt(((var1+var2)/2))))
  
}

##Helper function that sets up our simulation
simDvP = function(simParams) {
  
  #sim params is an input vector that contains the following (in this exact order):
  #Control group n, Treatment group n, pop mean of Control group, pop mean of Treatment group, pop sd of Control group, pop sd of Treatment group, repetitions for the sim
  
  set.seed(simParams[7]) #set seed equal to some number, in this case the number of repetitions; this is arbitrary
  reps = simParams[7] #
  outputMat = matrix(NA, nrow = reps, ncol = 6, dimnames = list(c(), c("controlMean", "treatMean", "controlSD", "treatSD", "pVal", "d")))
  
  for (r in 1:reps) {
    
    sampControl = rnorm(simParams[1], simParams[3], simParams[5]) #nControl, muControl, sigmaControl
    sampTreat = rnorm(simParams[2], simParams[4], simParams[6]) #nTreat, muTreat, sigmaTreat
    
    sampControlMean = mean(sampControl)
    sampTreatMean = mean(sampTreat)
    sampControlSD = sd(sampControl)
    sampTreatSD = sd(sampTreat)
    pVal = t.test(sampControl, sampTreat, paired=FALSE)$p.value
    d = effectD(sampControl, sampTreat)
    
    outputMat[r,] = c(sampControlMean, sampTreatMean, sampControlSD, sampTreatSD, pVal, d)
    
    
  }
  
  return(summary(outputMat))
  
}




nControl = 7
nTreat = 7
muControl = 17
muTreat = 16.5
sigmaControl = 1.2
sigmaTreat = 1.2
repetitions = 1000

simParams = c(nControl, nTreat, muControl, muTreat, sigmaControl, sigmaTreat, repetitions)


simDvP(simParams)
effectDPop(muControl, muTreat, sigmaControl^2, sigmaTreat^2)





