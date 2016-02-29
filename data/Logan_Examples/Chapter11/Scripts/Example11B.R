#################################################
### chunk number 1: pg 302
###################################################
andrew.patch <- with(andrew, aggregate(data.frame(ALGAE),
by=list(TREAT=TREAT, 
PATCH=PATCH), mean))
#OR
library(nlme)
andrew.patch <- gsummary(andrew, groups=andrew$PATCH)


###################################################
### chunk number 2: pg 303
###################################################
oneway.test(ALGAE~TREAT, andrew.patch, var.equal=F)


###################################################
### chunk number 3: pg 303
###################################################
summary(aov(rank(ALGAE)~TREAT+Error(PATCH), andrew))
