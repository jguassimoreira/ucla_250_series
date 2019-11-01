###########
# T TESTS #
###########
?t.test

#let's use the built-in sleep dataset
?sleep
sleep

#test for homogeneity of variance; requires car package
install.packages("car")
library(car)
?leveneTest #requires 
leveneTest(sleep$extra,sleep$group) #not significantly different

#separate vectors for two drugs
extra1=sleep$extra[1:10]
extra2=sleep$extra[11:20]

#run the defaults
t.test(extra1,extra2)
#Welch's two-sample t-test; assumes unequal variances
#from ?t.test, the default is var.equal=FALSE
#note non-integer df

#let's set var.equal to TRUE
t.test(extra1,extra2,var.equal=TRUE)
#not much of a difference...

var(extra1)
var(extra2)
#... because the variances are similar

#t test "by hand" in R
n1=length(extra1)
n2=length(extra2)
m1=mean(extra1)
m2=mean(extra2)
v1=var(extra1)
v2=var(extra2)

#numerator
numerator=m1-m2

#denominator
vpooled=(v1+v2)/2
se=vpooled/n1 #same n for both groups
denominator=sqrt(se+se)

numerator/denominator #same as above!

#many ways to make this t-test significant
#paired sample t-test
t.test(extra1,extra2,paired=TRUE,var.equal=TRUE)
#why is it significant?
cor(extra1,extra2) #because the scores are correlated
#but this correlation is meaningless because
?sleep
#separate people were in the two groups

#one-sided test
t.test(extra1,extra2,alternative="less",var.equal=TRUE)
#check out that one-sided confidence interval
#just for fun
t.test(extra1,extra2,alternative="greater",var.equal=TRUE)
#p=.96!

#change confidence level for CI
t.test(extra1,extra2,var.equal=TRUE,conf.level=.9)

#you can also do one-sample t tests by not using one dataset
# and giving a value for mu (default 0)
t.test(extra1,mu=0) #drug 1 doesn't work
t.test(extra2,mu=0) #but drug 2 does!

#reshape data
?reshape
sleep
sleep.wide=reshape(sleep,v.names="extra",idvar="ID",timevar="group",direction="wide")
sleep.wide

#save out dataset for SPSS
?write.csv
write.csv(sleep.wide,file=c("sleep.csv"),quote=FALSE)