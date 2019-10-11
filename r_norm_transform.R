#raincloud plots: https://wellcomeopenresearch.org/articles/4-63/v1
library(KernSmooth)

set.seed(17)
rt = sn::rmsn(100, c(5,-5), matrix(c(3,2,2,3)/3, 2, 2), c(10, 0.9))[,1]

rt.sort = sort(rt)
hist(rt.sort, breaks = 25)
c = hist(rt.sort, breaks = 25)$counts; d = hist(rt.sort, breaks = 25)$density
lines(rt.sort, dnorm(rt.sort, mean(rt.sort), sd(rt.sort)) * (c/d)[1])

hist(rt)
hist(rt, breaks = length(rt))
hist(rt, breaks = c(4:9))

rtSM_25 = bkde(rt, bandwidth = .25)
plot(rtSM_25, type="l")
rtSM_75 = bkde(rt, bandwidth = .75)
plot(rtSM_75, type="l")
rtSM_05 = bkde(rt, bandwidth = .05)
plot(rtSM_05, type="l")

hist(log(rt))
hist(log10(rt), breaks=25)
c10 =  hist(log10(rt), breaks = 25)$counts; d10 = hist(log10(rt), breaks = 25)$density
lines(sort(log10(rt)), dnorm(sort(log10(rt)), mean(log10(rt)), sd(log10(rt))) * (c10/d10)[1])
hist(rt^-3.218)


