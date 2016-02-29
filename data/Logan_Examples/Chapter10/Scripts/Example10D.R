###################################################
### chunk number 1: pg 273
###################################################
medley <- read.table('medley.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 273
###################################################
boxplot(DIVERSITY~STREAM, medley)


###################################################
### chunk number 3: pg 274
###################################################
medley.aov <- aov(DIVERSITY~STREAM, medley)
anova(medley.aov)


###################################################
### chunk number 4: pg 274
###################################################
library(nlme)
print(VarCorr(lme(DIVERSITY~1,  random=~1 | STREAM, method="ML", data=medley)))
print(VarCorr(lme(DIVERSITY~1,  random=~1 | STREAM, method="REML", data=medley)))
