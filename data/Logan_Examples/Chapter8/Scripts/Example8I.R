###################################################
### chunk number 1: pg 207
###################################################
library(pwr)
pwr.r.test(r=0.5,power=0.99)


###################################################
### chunk number 2: pg 207
###################################################
library(pwr)
r<-seq(.4,.9,l=100)
plot(sapply(r,function(x) pwr.r.test(r=x, power=.80)$n)~r, type='l', lwd=2, 
xlab="Correlation coefficient", ylab="Sample size")
points(sapply(r,function(x) pwr.r.test(r=x, power=.90)$n)~r, type='l')
points(sapply(r,function(x) pwr.r.test(r=x, power=.85)$n)~r, type='l')
points(sapply(r,function(x) pwr.r.test(r=x, power=.75)$n)~r, type='l')
