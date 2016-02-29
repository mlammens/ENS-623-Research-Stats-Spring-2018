###################################################
### chunk number 1: pg 498
###################################################
polis <- read.table('polis.csv', header=T,sep=',')


###################################################
### chunk number 2: pg 498
###################################################
polis.glm <- glm(PA~RATIO, family=binomial, data=polis)


###################################################
### chunk number 3: pg 498
###################################################
library(Design)


###################################################
### chunk number 4: pg 498
###################################################
polis.lrm<-lrm(PA~RATIO,data=polis,y=T, x=T)
resid(polis.lrm,type='gof')


###################################################
### chunk number 5: pg 499
###################################################
pp<-sum(resid(polis.lrm,type='pearson')^2)
1-pchisq(pp,polis.glm$df.resid)


###################################################
### chunk number 6: pg 499
###################################################
1-pchisq(polis.glm$deviance, polis.glm$df.resid)


###################################################
### chunk number 7: pg 499
###################################################
pp/polis.glm$df.resid


###################################################
### chunk number 8: pg 499
###################################################
library(car)
cr.plots(polis.glm, ask=F)


###################################################
### chunk number 9: pg 499
###################################################
influence.measures(polis.glm)


###################################################
### chunk number 10: pg 500
###################################################
summary(polis.glm)


###################################################
### chunk number 11: pg 501
###################################################
anova(polis.glm, test="Chisq")


###################################################
### chunk number 12: pg 501
###################################################
library(biology)
odds.ratio(polis.glm)


###################################################
### chunk number 13: pg 501
###################################################
1-(polis.glm$dev/polis.glm$null)


###################################################
### chunk number 14: pg 501
###################################################
-polis.glm$coef[1]/polis.glm$coef[2]


###################################################
### chunk number 15: pg 502
###################################################
# Calculate predicted values based on fitted model
xs<-seq(0,70,l=1000)
polis.predict <- predict(polis.glm, type="response", se=T, newdata=data.frame(RATIO=xs))
# Produce base plot
plot(PA~RATIO, data=polis, xlab="", ylab="", axes=F, pch=16)
# Plot fitted model and 95% CI bands
points(polis.predict$fit~xs, type="l", col="gray")
lines(polis.predict$fit+polis.predict$se.fit ~ xs, col="gray", type="l", lty=2)
lines(polis.predict$fit-polis.predict$se.fit ~ xs, col="gray", type="l", lty=2)
# Axes titles
mtext(expression(paste(italic(Uta), "presence/absence")), 2, line=3)
axis(2,las=1)
mtext("Permenter to area ratio",1, line=3)
axis(1)
box(bty="l")
