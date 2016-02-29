###################################################
### chunk number 1: pg 503
###################################################
bolger <- read.table('bolger.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 503
###################################################
library(car)
scatterplot.matrix(~RODENTSP+DISTX+AGE+PERSHRUB, data=bolger)


###################################################
### chunk number 3: pg 503
###################################################
bolger.glm <- glm(RODENTSP~DISTX+AGE+PERSHRUB, family=binomial, data=bolger)
vif(bolger.glm)


###################################################
### chunk number 4: pg 504
###################################################
library(Design)
bolger.lrm<-lrm(RODENTSP~DISTX+AGE+PERSHRUB, data=bolger, y=T, x=T)
resid(bolger.lrm,type='gof')


###################################################
### chunk number 5: pg 504
###################################################
pp<-sum(resid(bolger.lrm,type='pearson')^2)
1-pchisq(pp,bolger.glm$df.resid)


###################################################
### chunk number 6: pg 504
###################################################
1-pchisq(bolger.glm$deviance, bolger.glm$df.resid)


###################################################
### chunk number 7: pg 504
###################################################
pp/bolger.glm$df.resid


###################################################
### chunk number 8: pg 504
###################################################
library(car)
cr.plots(bolger.glm, ask=F)


###################################################
### chunk number 9: pg 505
###################################################
influence.measures(bolger.glm)


###################################################
### chunk number 10: pg 506
###################################################
summary(bolger.glm)


###################################################
### chunk number 11: pg 507
###################################################
# saturated model
bolger.glmS <- glm(RODENTSP ~ DISTX + AGE + PERSHRUB, family = binomial, data = bolger)
# Reduced model for distance
bolger.glm.Dist <- glm(RODENTSP ~ AGE + PERSHRUB, family = binomial, data = bolger)
#OR
bolger.glm.Dist <- update(bolger.glmS, "~.-DISTX")
anova(bolger.glmS, bolger.glm.Dist, test = "Chisq")


###################################################
### chunk number 12: pg 507
###################################################
# Reduced model for age
bolger.glm.Age <- update(bolger.glmS, "~.-AGE")
anova(bolger.glmS, bolger.glm.Age, test = "Chisq")


###################################################
### chunk number 13: pg  507
###################################################
# Reduced model for shrub cover
bolger.glm.Shrub <- update(bolger.glmS, "~.-PERSHRUB")
anova(bolger.glmS, bolger.glm.Shrub, test = "Chisq")


###################################################
### chunk number 14: pg 508
###################################################
library(biology)
odds.ratio(bolger.glm)


###################################################
### chunk number 15: pg 508
###################################################
library(biology)
Model.selection.glm(bolger.glm)


###################################################
### chunk number 16: pg 509
###################################################
bolger.glm <- glm(RODENTSP~PERSHRUB, family=binomial, data=bolger)
summary(bolger.glm)


###################################################
### chunk number 17: pg 510
###################################################
# Calculate predicted values based on fitted model
xs<-seq(0,100,l=1000)
bolger.predict <- with(bolger, (predict(bolger.glm, type="response", se=T, newdata=data.frame(DISTX=mean(DISTX), AGE=mean(AGE), PERSHRUB=xs))))
# Produce base plot
plot(RODENTSP~PERSHRUB, data=bolger, xlab="", ylab="", axes=F, pch=16)
# Plot fitted model and 95% CI bands
points(bolger.predict$fit~xs, type="l", col="gray")
lines(bolger.predict$fit+bolger.predict$se.fit ~ xs, col="gray", type="l", lty=2)
lines(bolger.predict$fit-bolger.predict$se.fit ~ xs, col="gray", type="l", lty=2)
# Axes titles
mtext("Native rodent presence/absence", 2, line=3)
axis(2,las=1)
mtext("Percentage shrub cover",1, line=3)
axis(1)
box(bty="l")
