library(KernSmooth)

set.seed(17) #set a seed for reproducibility 
rt = sn::rmsn(100, c(5,-5), matrix(c(3,2,2,3)/3, 2, 2), c(10, 0.9))[,1] #sim data from a skewed bivariate normal distribution, get only the 1st variable


hist(rt, breaks = 25, freq = F) #make a histogram
lines(density(rt)) #overlays the density
#This is a clunkier way of overlaying the density that might work in other cases, but I won't go over, nor do I recommend unless you do some digging
#c = hist(rt, breaks = 25)$counts; d = hist(rt, breaks = 25)$density #this is going to help us overlay the density
#lines(rt, dnorm(rt, mean(rt), sd(rt)) * (c/d)[1])

#playing with the breaks in the histogram
hist(rt)
hist(rt, breaks = length(rt))
hist(rt, breaks = c(4:9))

#kernel smoothing stuff
rtSM_25 = bkde(rt, bandwidth = .25)
plot(rtSM_25, type="l")
rtSM_75 = bkde(rt, bandwidth = .75)
plot(rtSM_75, type="l")
rtSM_05 = bkde(rt, bandwidth = .05)
plot(rtSM_05, type="l")

#log transformation stuff
hist(log(rt))
hist(log10(rt), breaks=25)
c10 =  hist(log10(rt), breaks = 25)$counts; d10 = hist(log10(rt), breaks = 25)$density
lines(sort(log10(rt)), dnorm(sort(log10(rt)), mean(log10(rt)), sd(log10(rt))) * (c10/d10)[1])
hist(rt^-3.218)


