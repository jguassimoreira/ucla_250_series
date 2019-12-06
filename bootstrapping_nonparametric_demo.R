########################################
########################################
##  Bootstrapping  By Hand
########################################
########################################

##### bootstrap confidence interval for a mean

library(sn)
library(psych)

N=20
skew=20                           # skewness parameters (for disctribution of X)        
delta=skew/sqrt(skew^2+1)         # to ensure that E(X)=0 and VAR(X)=1
omega=1/sqrt(1-2*delta^2/pi)
kappa=-omega*sqrt(2/pi)*delta

set.seed(59)
X = rsn(N,kappa,omega,skew)         #draw X from skew normal distribution

describe(X)

CITL <- mean(X) + qt(.025, df=N-1) * sd(X)/sqrt(N)
CITH <- mean(X) + qt(.975, df=N-1) * sd(X)/sqrt(N)

outcome <- matrix(0,10000,1)

for (i in 1:10000) {
  samp <- sample(X, size=N, replace = TRUE)
  outcome[i] <- mean(samp)
}

mean(outcome)
EQ <- quantile(outcome, c(.025, .975))

hist(outcome, breaks=100, freq=FALSE, main="Red is Parametric CI, Blue is Bootstrapped CI", xlab="Sample Means")
abline(v=CITL, lwd=2, col="red")
abline(v= CITH, lwd=2, col="red")

curve(dnorm(x, mean(outcome[,1]), sd=(sd(X)/sqrt(N))), add = TRUE)
abline(v=EQ[1], lwd=2, col="blue")
abline(v=EQ[2], lwd=2, col="blue")


##### bootstrap confidence interval for a median

library(sn)
library(psych)

N=20
skew=20                           # skewness parameters (for disctribution of X)        
delta=skew/sqrt(skew^2+1)         # to ensure that E(X)=0 and VAR(X)=1
omega=1/sqrt(1-2*delta^2/pi)
kappa=-omega*sqrt(2/pi)*delta

X = rsn(N,kappa,omega,skew)         #draw X from skew normal distribution

set.seed(2011)
describe(X)

outcome <- matrix(0,10000,1)

for (i in 1:10000) {
  samp <- sample(X, size=N, replace = TRUE)
  outcome[i] <- median(samp)
}

EQ <- quantile(outcome, c(.025, .975))

hist(outcome, breaks=20, freq=FALSE, main="Blue is Bootstrapped CI", xlab="Sample Medians")
abline(v=EQ[1], lwd=2, col="blue")
abline(v=EQ[2], lwd=2, col="blue")
abline(v=median(X), lwd=2, col="black")


##### bootstrap confidence interval for a skew

library(sn)
library(psych)

N=20
skew=20                           # skewness parameters (for disctribution of X)        
delta=skew/sqrt(skew^2+1)         # to ensure that E(X)=0 and VAR(X)=1
omega=1/sqrt(1-2*delta^2/pi)
kappa=-omega*sqrt(2/pi)*delta

set.seed(2013+2015) #Try with set.seed(20132015)
X = rsn(N,kappa,omega,skew)         #draw ability from skew normal distribution

describe(X)

outcome <- matrix(0,10000,1)

for (i in 1:10000) {
  samp <- sample(X, size=N, replace = TRUE)
  outcome[i] <- skew(samp)
}

mean(outcome)
EQ <- quantile(outcome, c(.025, .975))

hist(outcome, breaks=20, freq=FALSE, main="Blue is Bootstrapped CI", xlab="Sample Skews")
abline(v=EQ[1], lwd=2, col="blue")
abline(v=EQ[2], lwd=2, col="blue")
abline(v=skew(X), lwd=2, col="black")

#######################################
## Bootstrapped Correlation
#######################################

library(MASS)

true <- matrix(1,2,2)
true[1,2] <- .8; true[2,1] <- .8

N = 500

set.seed(4)
X <- mvrnorm(n=N, mu = c(0, 0), Sigma=true)
plot(X[,1], X[,2])
sampresult <- cor(X)

index <- seq(1,nrow(X),1)

outcome <- matrix(0,10000,1)

for (i in 1:10000) {
  samp <- sample(index, size=N, replace = TRUE)
  outcome[i] <- cor(X[samp,1],X[samp,2])
}

EQ <- quantile(outcome, c(.025, .975))

hist(outcome, breaks=20, freq=FALSE, main="Blue is Bootstrapped CI", xlab="Sample Correlations, true is .80")
abline(v=EQ[1], lwd=2, col="blue")
abline(v=EQ[2], lwd=2, col="blue")
abline(v=samresult, lwd=2, col="black")


##################################################
##### bootstrap confidence interval for Cohen's D
##################################################

n1 <- 250; n2 <- 250

set.seed(1)
X1 <- rnorm(n1, mean=50, sd=10)
X2 <- rnorm(n2, mean=55, sd=10)

describe(X1); describe(X2)

Sp <- sqrt(((n1-1) * var(X1) + (n2-1) * var(X2)) / (n1 + n2 - 2))
samcohenD <- (mean(X1) - mean(X2)) / Sp


outcome <- matrix(0,10000,1)

for (i in 1:10000) {
  samp1 <- sample(X1, size=n1, replace = TRUE)
  samp2 <- sample(X2, size=n2, replace = TRUE)
  Sp <- sqrt(((n1-1) * var(samp1) + (n2-1) * var(samp2)) / (n1 + n2 - 2))
  cohenD <- (mean(samp1) - mean(samp2)) / Sp
  outcome[i] <- cohenD
}

EQ <- quantile(outcome, c(.025, .975))

hist(outcome, breaks=20, freq=FALSE, main="Blue is Bootstrapped CI", xlab="Sample Cohen D, true in population = -0.5")
abline(v=EQ[1], lwd=2, col="blue")
abline(v=EQ[2], lwd=2, col="blue")
abline(v=samcohenD, lwd=2, col="black")
abline(v=-0.5, lwd=2, col="red")


##############################################################################
##### Randomization Test of Differences in Paired Samples Design
##############################################################################

n <- 20
set.seed(100) #Try 100, 105, 110
X1 = x[,1]; X2 = x[,2]
X1 = X1 + runif(n, -5, 5); X2 = X2 + runif(n, -5, 5)

describe(X1); describe(X2)


X1 = rnorm(n, 50, 10)
X2 = X1 + rnorm(n, 2, 5)

diffscores <- X1 - X2

Mdiff <- mean(diffscores)

outcome <- matrix(0,10000,1)

for (i in 1:10000) {
  index <- runif(n1,0,1)
  
  sam <- ifelse( (index <= .50), -1*diffscores, 1 *diffscores)
  
  outcome[i] <- mean(sam)
}

EQ <- quantile(outcome, c(.025, .975))

hist(outcome, breaks=20, freq=FALSE, main="Blue corresponds to two-tailed p .05", xlab="Mean of difference scores")
abline(v=EQ[1], lwd=2, col="blue")
abline(v=EQ[2], lwd=2, col="blue")
abline(v=Mdiff, lwd=2, col="black")



###############################################################################
##### Randomization Test of Differences in Mean for Two-Group Independent Design
###############################################################################

n1 <- 15; n2 <- 30

X1 <- rnorm(n1, mean=50, sd=7);  X2 <- rnorm(n2, mean=55, sd=14)

describe(X1); describe(X2)

meandiff <- mean(X1) - mean(X2)

outcome <- matrix(0,10000,1)

all <- c(X1,X2)  # stack the two columns to make one sample

for (i in 1:10000) {
  
  index <- sample(1:(n1+n2), size=(n1), replace = FALSE)
  grp1 <- all[index]
  grp2 <- all[-index]
  outcome[i] <- mean(grp1) - mean(grp2)
}

mean(outcome)
EQ <- quantile(outcome, c(.025, .975))

hist(outcome, breaks=20, freq=FALSE, main="Blue is Bootstrapped CI", xlab="Mean Difference, true in population = -0.5")
abline(v=EQ[1], lwd=2, col="blue")
abline(v=EQ[2], lwd=2, col="blue")
abline(v=meandiff, lwd=2, col="black")

Fn <- ecdf(outcome) # the cummulative function

Fn(meandiff)  # returns the percentiles for meandiff


