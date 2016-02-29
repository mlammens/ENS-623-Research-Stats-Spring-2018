###################################################
### chunk number 1: pg 299
###################################################
andrew <- read.table('andrew.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 299
###################################################
class(andrew$PATCH)


###################################################
### chunk number 3: pg 299
###################################################
andrew$PATCH <-factor(andrew$PATCH)
class(andrew$PATCH)


###################################################
### chunk number 4: pg 299
###################################################
levels(andrew$TREAT)
andrew$TREAT<-factor(andrew$TREAT, levels=c("100%", "66%", "33%", "0%"))


###################################################
### chunk number 5: pg 299
###################################################
andrew.agg <- with(andrew, aggregate(data.frame(ALGAE), by=list(TREAT=TREAT, 
PATCH=PATCH), mean))
#OR
library(nlme)
andrew.agg <- gsummary(andrew, groups=andrew$PATCH)


###################################################
### chunk number 6: pg 300
###################################################
boxplot(ALGAE~TREAT, andrew.agg)


###################################################
### chunk number 7: pg 300
###################################################
replications(ALGAE~TREAT+PATCH, andrew)


###################################################
### chunk number 8: pg 300
###################################################
library(biology)
is.balanced(ALGAE~TREAT+PATCH, andrew)


###################################################
### chunk number 9: pg 300
###################################################
contrasts(andrew$TREAT) <- contr.treatment


###################################################
### chunk number 10: pg 301
###################################################
andrew.aov <- aov(ALGAE~TREAT+Error(PATCH), andrew)


###################################################
### chunk number 11: pg 301
###################################################
plot(resid(andrew.aov[[2]])~fitted(andrew.aov[[2]]))


###################################################
### chunk number 12: pg 301
###################################################
summary(andrew.aov, split=list(TREAT=list('cont vs 66'=1, 'cont vs 33'=2, 'cont vs 0'=3)))


###################################################
### chunk number 13: pg 302
###################################################
library(nlme)
VarCorr(lme(ALGAE~1,random=~1|TREAT/PATCH, andrew))
