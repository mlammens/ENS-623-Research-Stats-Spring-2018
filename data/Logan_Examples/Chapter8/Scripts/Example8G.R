###################################################
### chunk number 1: pg 201
###################################################
smith <- read.table('smith.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 201
###################################################
library(car)
scatterplot(RATIO~YEARS,smith)


###################################################
### chunk number 3: pg 202
###################################################
library(mblm)
smith.mblm<-mblm(RATIO~YEARS, smith, repeated=F)
summary(smith.mblm)


###################################################
### chunk number 4: pg 203
###################################################
confint.mblm(smith.mblm, level=0.95)
