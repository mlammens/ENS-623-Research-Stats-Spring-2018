###################################################
### chunk number 1: pg 197
###################################################
beetles <- read.table('beetles.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 197
###################################################
library(car)
scatterplot(SURVIVAL~DENSITY, 
data=beetles)


###################################################
### chunk number 3: pg 197
###################################################
boxplot(SURVIVAL~DENSITY, data=beetles)


###################################################
### chunk number 4: pg 197
###################################################
anova(lm(SURVIVAL~DENSITY + as.factor(DENSITY), beetles))


###################################################
### chunk number 5: pg 198
###################################################
#calculate critical F for alpha=0.25, df=2,11
qf(0.25,2,11, lower=T)


###################################################
### chunk number 6: pg 198
###################################################
beetles.lm <- aov(SURVIVAL~DENSITY+Error(as.factor(DENSITY)), beetles)
summary(beetles.lm)


###################################################
### chunk number 7: pg 198
###################################################
#to get the regression coefficients
lm(SURVIVAL~DENSITY, beetles)


###################################################
### chunk number 8: pg 198
###################################################
summary(lm(SURVIVAL~DENSITY, beetles))


###################################################
### chunk number 9: pg 199
###################################################
beetles$DENSITY<-as.factor(beetles$DENSITY)
contrasts(beetles$DENSITY)<-contr.poly(4,c(5,20,50,100))
beetles.aov<-aov(SURVIVAL~DENSITY, beetles)
summary(beetles.aov,split=list(DENSITY=list(1,c(2,3))))

