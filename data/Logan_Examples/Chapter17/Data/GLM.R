###################################################
### chunk number 1: GLM_SetDir
###################################################
setwd("GLM/Data")
setCacheDir("../cache")
options(SweaveHooks=list(fig=function() par(mar=c(4,4,0.1,0.1))) )


###################################################
### chunk number 2: logisticRegression1
###################################################
getOption("SweaveHooks")[["fig"]]()
source("../../Figure_helper.R")
opar<-par(mar=c(3,3,.1,4), mfrow=c(2,2))
x <- 1:20
y <- c(0,0,0,0,0,0,0,0,0,1,0,1,0,1,1,1,1,1,1,1)
lm.1<-lm(y~x)
plot(y~x, ylim=c(-.2,1.2), type="p",pch=16, xlab="", ylab="",axes=F)
abline(lm.1, col="gray", lwd=2)
  segments(x,predict(lm.1),x,y, col="gray")
  axis(1,tick=F,lab=F)
  axis(2,las=0, at=c(0,1),lab=c("Absent","Present"))
  axis(4, las=1, col="gray", at=c(0,.2,.4,.6,.8,1),bg="gray")
  mtext("Predicted probability\nof presence",4, line=4, col="gray")
  text(upperLeft(offset=0.05),"a)", cex=1)
  mtext("X",1, line=1.5)
  box(bty="U", col="gray")
  box(bty="c")

  opar<-par(mar=c(3,4,.1,3))
  x <- 1:20
  y <- c(0,0,0,0,0,0,0,0,0,1,0,1,0,1,1,1,1,1,1,1)
                                        #plot(lm.1<-lm(y~x), 1,ask=F)
                                        #hist(resid(lm.1))
  plot(y~x, ylim=c(-.2,1.2), type="p",pch=16, xlab="", ylab="",axes=F)
  xx <- seq(0,20,l=1000)
                                        #points(xx,plogis(xx, 10.5), type="l")
#points(xx,(glm(y~x,family="binomial"))$fitted, type="l")
                                        #points(xx,predict(glm(y~x,family="binomial"),data.frame(x=xx)), type="l")
                                        #abline(lm.1, col="gray", lwd=2)
  points(glm(y~x,family="binomial")$fitted~x, type="l", col="Gray")
                                        #abline(glm(y~x,family="binomial"))
  segments(x,(glm(y~x,family="binomial"))$fitted,x,y, col="gray")
  axis(1,tick=F,lab=F)
  axis(4,las=0, at=c(0,1),lab=c("Absent","Present"))
  axis(2, las=1, col="gray", at=c(0,.2,.4,.6,.8,1),bg="gray")
                                        #mtext("Predicted probability\nof presence",4, line=4, col="gray")
  box(bty="U", col="gray")
  box(bty="]")
  text(upperLeft(offset=0.05),"b)", cex=1)
  mtext("X",1, line=1.5)
  
  opar<-par(mar=c(3,3,.1,4))
  x <- seq(0,10,l=1000)
  plot(x,plogis(x, 5), type="l", axes=F, xlab="",ylab="")
  text(upperLeft(offset=0.05),"c)", cex=1)
  box(bty="l")
  
  opar<-par(mar=c(3,4,.1,3))
                                        #opar<-par(mar=c(4,.1,.1,.1))
  x<-0:50
  y<-dbinom(x,25, prob=.5)
  y1<-dbinom(x,15, prob=.5)
  y2<-dbinom(x,5, prob=.5)
  plot(x,y,type='h',ylim=c(0,max(y)), xlim=c(0,25), axes=F, xlab="", ylab="", lwd=5, col="gray80")
                                        #lines(x,y1, type='h',lwd=5, col="gray50")
                                        #lines(x,y2, type='h',lwd=5, col="gray0")
                                        #axis(1, at=seq(0,50,b=5))
                                        #text(upperLeft(offset=0.05),"a)", cex=1)
  text(upperLeft(offset=0.05),"d)", cex=1)
  box(bty="l")
  par(opar)


###################################################
### chunk number 3: GAM2
###################################################
getOption("SweaveHooks")[["fig"]]()
  peake1 <-
structure(list(SPECIES = c(3L, 
7L, 6L, 8L, 10L, 9L, 10L, 11L, 16L, 9L, 13L, 14L, 12L, 14L, 20L, 
22L, 19L, 20L, 22L, 21L, 24L, 24L, 25L, 25L, 24L), INDIV = c(18L, 
60L, 57L, 100L, 48L, 118L, 148L, 214L, 225L, 283L, 380L, 278L, 
338L, 274L, 569L, 509L, 682L, 600L, 978L, 363L, 1402L, 675L, 
1159L, 1062L, 632L)), .Names = c("SPECIES", "INDIV"), class = "data.frame", row.names = c(NA, 
-25L))
opar<-par(mar=c(3,3,.11,4), mfrow=c(2,2))
#linear model
plot(SPECIES ~ INDIV, data=peake1, ylim=c(0,30), axes=F, xlab="", ylab="")
points(SPECIES ~ INDIV, data=peake1, pch=16)
mtext("X",1, line=1)
axis(1,tick=F,lab=F)
mtext("Y",2, line=1)
axis(2,tick=F,lab=F)
xs <- with(peake1,seq(min(INDIV), max(INDIV), length=500))
peake.lm<-lm(SPECIES ~ INDIV, data=peake1)
peake.lm.pred <- predict(peake.lm, data.frame(INDIV=xs), se=TRUE)
lines(xs, peake.lm.pred$fit, lwd=3)
lines(xs, peake.lm.pred$fit + 2*peake.lm.pred$se.fit, lty=2, lwd=2, col="gray")
lines(xs, peake.lm.pred$fit - 2*peake.lm.pred$se.fit, lty=2, lwd=2, col="gray")
text(500,10,"Linear regression line", pos=4)
box(bty="l")
text(upperLeft(offset=0.05),"a)", cex=1)

#linear model residuals
plot(resid(peake.lm)~fitted(peake.lm),axes=F, xlab="", ylab="", type="n")
points(resid(peake.lm)~fitted(peake.lm), pch=16)
mtext("Predicted Y values",1, line=1)
axis(1,tick=F,lab=F)
mtext("Residuals",2, line=1)
axis(2,tick=F,lab=F)
lines(loess.smooth(fitted(peake.lm), resid(peake.lm), span=.5, degree=1), lty=2, lwd=2, col="gray")
abline(h=0, col="gray")
box(bty="l")
text(upperLeft(offset=0.05),"b)", cex=1)

#cubic spline model
library(splines)
plot(SPECIES ~ INDIV, data=peake1, ylim=c(0,30), axes=F, xlab="", ylab="")
points(SPECIES ~ INDIV, data=peake1, pch=16)
mtext("X",1, line=1)
axis(1,tick=F,lab=F)
mtext("Y",2, line=1)
axis(2,tick=F,lab=F)
peake.sp <- lm(SPECIES ~ ns(INDIV, knots=500),data=peake1)
peake.sp.pred <- predict(peake.sp, data.frame(INDIV=xs), se=TRUE)
lines(xs, peake.sp.pred$fit, lwd=3)
lines(xs, peake.sp.pred$fit + 2*peake.sp.pred$se.fit, lty=2, lwd=2, col="gray")
lines(xs, peake.sp.pred$fit - 2*peake.sp.pred$se.fit, lty=2, lwd=2, col="gray")
text(500,10,"Cubic spline smoother", pos=4)
box(bty="l")
text(upperLeft(offset=0.05),"c)", cex=1)

plot(resid(peake.sp)~fitted(peake.sp),axes=F, xlab="", ylab="", type="n")
points(resid(peake.sp)~fitted(peake.sp), pch=16)
mtext("Predicted Y values",1, line=1)
axis(1,tick=F,lab=F)
mtext("Residuals",2, line=1)
axis(2,tick=F,lab=F)
lines(loess.smooth(fitted(peake.sp), resid(peake.sp), span=.5, degree=1), lty=2, lwd=2, col="gray")
abline(h=0, col="gray")
box(bty="l")
text(upperLeft(offset=0.05),"d)", cex=1)

## #cubic spline model
## library(splines)
## plot(SPECIES ~ INDIV, data=peake1, ylim=c(0,30), axes=F, xlab="", ylab="")
## points(SPECIES ~ INDIV, data=peake1, pch=16)
## mtext("X",1, line=1)
## axis(1,tick=F,lab=F)
## mtext("Y",2, line=1)
## axis(2,tick=F,lab=F)
## peake.lo <- loess(SPECIES ~ INDIV,data=peake1, span=0.7)
## peake.lo.pred <- predict(peake.lo, data.frame(INDIV=xs), se=TRUE)
## lines(xs, peake.lo.pred$fit, lwd=3)
## lines(xs, peake.lo.pred$fit + 2*peake.lo.pred$se.fit, lty=2, lwd=2, col="gray")
## lines(xs, peake.lo.pred$fit - 2*peake.lo.pred$se.fit, lty=2, lwd=2, col="gray")
## text(500,10,"Loess smoother", pos=4)
## box(bty="l")
## text(upperLeft(offset=0.05),"e)", cex=1)

## plot(resid(peake.lo)~fitted(peake.lo),axes=F, xlab="", ylab="", type="n")
## points(resid(peake.lo)~fitted(peake.lo), pch=16)
## mtext("Predicted Y values",1, line=1)
## axis(1,tick=F,lab=F)
## mtext("Residuals",2, line=1)
## axis(2,tick=F,lab=F)
## lines(loess.smooth(fitted(peake.lo), resid(peake.lo), span=.5, degree=1), lty=2, lwd=2, col="gray")
## abline(h=0, col="gray")
## box(bty="l")
## text(upperLeft(offset=0.05),"f)", cex=1)
par(opar)


###################################################
### chunk number 4: GLMLogisiticRegression eval=FALSE
###################################################
## data.glm<-glm(DV~IV, dataset, family="Poisson")


###################################################
### chunk number 5: GLMLogisiticRegressionSummary eval=FALSE
###################################################
## summary(data.glm)


###################################################
### chunk number 6: GLMLogisiticRegressionDeviance eval=FALSE
###################################################
## anova(data.glm, test="Chisq")


###################################################
### chunk number 7: GLMMultipleLogisiticRegression eval=FALSE
###################################################
## data.glm<-glm(DV~IV1+IV2+..., dataset, family="Poisson")


###################################################
### chunk number 8: GLMLogisiticRegressionSummary eval=FALSE
###################################################
## summary(data.glm)


###################################################
### chunk number 9: GLMLogisiticRegressionDeviance1 eval=FALSE
###################################################
## anova(data.glm, data.glmR, test="Chisq")


###################################################
### chunk number 10: GLMAssumptionsLackOfFitleCessie eval=FALSE
###################################################
## library(Design)
## data.lrm <- lrm(formula, dataset, y=T, x=T)
## resid(data.lrm)


###################################################
### chunk number 11: GLMAssumptionsLackOfFitPearson eval=FALSE
###################################################
## pp <- sum(resid(data.lrm, type="pearson")^2)
## 1-pchisq(pp, data.glm$df.resid)


###################################################
### chunk number 12: GLMAssumptionsLackOfFitDeviance eval=FALSE
###################################################
## 1-pchisq(data.glm, data.glm$df.resid)


###################################################
### chunk number 13: GLMAssumptionsLinearity eval=FALSE
###################################################
## library(car)
## cr.plots(data.glm, ask=F)


###################################################
### chunk number 14: GLMAssumptionsInfluence eval=FALSE
###################################################
## influence.measures(data.glm)


###################################################
### chunk number 15: GLMLogisiticRegressionDispersionPearsons eval=FALSE
###################################################
## sum(resid(data.glm, type="pearson")^2)/data.glm$df.resid


###################################################
### chunk number 16: GLMLogisiticRegressionDispersionDeviance eval=FALSE
###################################################
## data.glm$deviance/data.glm$df.resid


###################################################
### chunk number 17: GLMLogisiticRegressionDispersionQuasi eval=FALSE
###################################################
## data.glm <- glm(DV~IV, dataset, family="quasibinomial")
## anova(data.glm, test="F")


###################################################
### chunk number 18: GLMLogisiticRegressionDispersionNB eval=FALSE
###################################################
## data.glm <- glm.nb(DV~IV, dataset)
## anova(data.glm, test="F")


###################################################
### chunk number 19: GLMPoissonRegression eval=FALSE
###################################################
## data.glm <- glm(DV~IV1+..., dataset, family="poisson")


###################################################
### chunk number 20: GLMLogisiticRegressionSummary eval=FALSE
###################################################
## summary(data.glm)


###################################################
### chunk number 21: GLMLogisiticRegressionDeviance1 eval=FALSE
###################################################
## anova(data.glm, data.glmR, test="Chisq")


###################################################
### chunk number 22: GLMLoglinearModelling eval=FALSE
###################################################
## data.glm <- glm(DV~CAT1*CAT2*..., dataset, family="poisson")


###################################################
### chunk number 23: GLMConditionalIndependence eval=FALSE
###################################################
## data.glm1 <- update(data.glm, ~.-CAT1:CAT2, dataset)
## anova(data.glm, data.glm1, test="Chisq")


###################################################
### chunk number 24: GLModdsRatios eval=FALSE
###################################################
## library(biology)
## odds.ratio(data.glm)


###################################################
### chunk number 25: GLMGAM eval=FALSE
###################################################
## library(gam)
## data.gam <- gam(DV~lo(CAT1)+lo(CAT2)+..., family="gaussian", dataset)


###################################################
### chunk number 26: GLMGAMEstimates eval=FALSE
###################################################
## sumamry(data.gam)


###################################################
### chunk number 27: GLMLogisiticRegressionModelAveraging1 eval=FALSE
###################################################
## library(MuMIn)
## dredge(data.glm)
## model.avg(get.models(dredge(model)))


###################################################
### chunk number 28: GLMLogisiticRegressionModelAveraging eval=FALSE
###################################################
## library(biology)
## Model.selection.glm(model)


###################################################
### chunk number 29: PolisReadTable
###################################################
polis <- read.table('polis.csv', header=T,sep=',')


###################################################
### chunk number 30: PolisGLM
###################################################
polis.glm <- glm(PA~RATIO, family=binomial, data=polis)


###################################################
### chunk number 31: libraryDesign
###################################################
library(Design)


###################################################
### chunk number 32: PolisResid
###################################################
polis.lrm<-lrm(PA~RATIO,data=polis,y=T, x=T)
resid(polis.lrm,type='gof')


###################################################
### chunk number 33: PolisPearson
###################################################
pp<-sum(resid(polis.lrm,type='pearson')^2)
1-pchisq(pp,polis.glm$df.resid)


###################################################
### chunk number 34: PolisDeviance
###################################################
1-pchisq(polis.glm$deviance, polis.glm$df.resid)


###################################################
### chunk number 35: PolisDispersion
###################################################
pp/polis.glm$df.resid


###################################################
### chunk number 36: polisComponentResidual
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
cr.plots(polis.glm, ask=F)


###################################################
### chunk number 37: PolisInfluenceMeasures
###################################################
influence.measures(polis.glm)


###################################################
### chunk number 38: PolisSummary
###################################################
summary(polis.glm)


###################################################
### chunk number 39: PolisAnova
###################################################
anova(polis.glm, test="Chisq")


###################################################
### chunk number 40: PolisOddsratios
###################################################
library(biology)
odds.ratio(polis.glm)


###################################################
### chunk number 41: PolisR2
###################################################
1-(polis.glm$dev/polis.glm$null)


###################################################
### chunk number 42: PolisLD50
###################################################
-polis.glm$coef[1]/polis.glm$coef[2]


###################################################
### chunk number 43: polis
###################################################
getOption("SweaveHooks")[["fig"]]()
# Calculate predicted values based on fitted model
xs<-seq(0,70,l=1000)
polis.predict <- predict(polis.glm, type="response", se=T,
newdata=data.frame(RATIO=xs))
# Produce base plot
plot(PA~RATIO, data=polis, xlab="", ylab="", axes=F,
pch=16)
# Plot fitted model and 95% CI bands
points(polis.predict$fit~xs, type="l", col="gray")
lines(polis.predict$fit+polis.predict$se.fit ~ xs, col="gray", type="l", lty=2)
lines(polis.predict$fit-polis.predict$se.fit ~ xs,
col="gray", type="l", lty=2)
# Axes titles
mtext(expression(paste(italic(Uta), "presence/absence")), 2, line=3)
axis(2,las=1)
mtext("Permenter to area ratio",1, line=3)
axis(1)
box(bty="l")


###################################################
### chunk number 44: BolgerReadTable
###################################################
bolger <- read.table('bolger.csv', header=T, sep=',')


###################################################
### chunk number 45: bolgerScatterPlot
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
scatterplot.matrix(~RODENTSP+DISTX+AGE+PERSHRUB, data=bolger)


###################################################
### chunk number 46: BolgerVIF
###################################################
bolger.glm <- glm(RODENTSP~DISTX+AGE+PERSHRUB, family=binomial, data=bolger)
vif(bolger.glm)


###################################################
### chunk number 47: BolgerCessie
###################################################
library(Design)
bolger.lrm<-lrm(RODENTSP~DISTX+AGE+PERSHRUB, data=bolger, y=T, x=T)
resid(bolger.lrm,type='gof')


###################################################
### chunk number 48: BolgerPearson
###################################################
pp<-sum(resid(bolger.lrm,type='pearson')^2)
1-pchisq(pp,bolger.glm$df.resid)


###################################################
### chunk number 49: BolgerDeviance
###################################################
1-pchisq(bolger.glm$deviance, bolger.glm$df.resid)


###################################################
### chunk number 50: BolgerDispersion
###################################################
pp/bolger.glm$df.resid


###################################################
### chunk number 51: bolgerComponentResidual
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
cr.plots(bolger.glm, ask=F)


###################################################
### chunk number 52: BolgerInfluenceMeasures eval=FALSE
###################################################
## influence.measures(bolger.glm)


###################################################
### chunk number 53: BolgerInfluenceMeasures
###################################################
txt <- capture.output( {
influence.measures(bolger.glm)
} )
if( length(txt) > 10 ){
    txt <- c( txt[3:29] )
}
cat( txt, sep = "\n" )


###################################################
### chunk number 54: BolgerSummary
###################################################
summary(bolger.glm)


###################################################
### chunk number 55: BolgerAnova1
###################################################
# saturated model
bolger.glmS <- glm(RODENTSP ~ DISTX + AGE + PERSHRUB, 
family = binomial, data = bolger)
# Reduced model for distance
bolger.glm.Dist <- glm(RODENTSP ~ AGE + PERSHRUB, 
family = binomial, data = bolger)
#OR
bolger.glm.Dist <- update(bolger.glmS, "~.-DISTX")
anova(bolger.glmS, bolger.glm.Dist, test = "Chisq")


###################################################
### chunk number 56: BolgerAnova2
###################################################
# Reduced model for age
bolger.glm.Age <- update(bolger.glmS, "~.-AGE")
anova(bolger.glmS, bolger.glm.Age, test = "Chisq")


###################################################
### chunk number 57: BolgerAnova3
###################################################
# Reduced model for shrub cover
bolger.glm.Shrub <- update(bolger.glmS, "~.-PERSHRUB")
anova(bolger.glmS, bolger.glm.Shrub, test = "Chisq")


###################################################
### chunk number 58: BolgerOddsratios
###################################################
library(biology)
odds.ratio(bolger.glm)


###################################################
### chunk number 59: BolgerModelSelection
###################################################
library(biology)
Model.selection.glm(bolger.glm)


###################################################
### chunk number 60: BolgerPredictiveModel
###################################################
bolger.glm <- glm(RODENTSP~PERSHRUB, family=binomial, data=bolger)
summary(bolger.glm)


###################################################
### chunk number 61: bolger
###################################################
getOption("SweaveHooks")[["fig"]]()
# Calculate predicted values based on fitted model
xs<-seq(0,100,l=1000)
bolger.predict <- with(bolger, (predict(bolger.glm, type="response", se=T,
newdata=data.frame(DISTX=mean(DISTX), AGE=mean(AGE), PERSHRUB=xs))))
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


###################################################
### chunk number 62: GotelliReadTable
###################################################
gotelli <- read.table('gotelli.csv', header=T, sep=',')


###################################################
### chunk number 63: GotelliCentering
###################################################
gotelli$cLatitude<-scale(gotelli$Latitude,scale=F)
gotelli$cElevation<-scale(gotelli$Elevation,scale=F)


###################################################
### chunk number 64: gotelliScatterPlot
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
scatterplot.matrix(~Srich+Habitat*cLatitude*cElevation, data=gotelli)


###################################################
### chunk number 65: GotelliVIF
###################################################
gotelli.glm <- glm(Srich~Habitat*cLatitude*cElevation, family=poisson,data=gotelli)
vif(gotelli.glm)


###################################################
### chunk number 66: GotelliPearson
###################################################
pp<-sum(resid(gotelli.glm,type='pearson')^2)
1-pchisq(pp,gotelli.glm$df.resid)


###################################################
### chunk number 67: GotelliDeviance
###################################################
1-pchisq(gotelli.glm$deviance, gotelli.glm$df.resid)


###################################################
### chunk number 68: GotelliInfluenceMeasures eval=FALSE
###################################################
## influence.measures(gotelli.glm)


###################################################
### chunk number 69: GotelliInfluenceMeasures2
###################################################
txt <- capture.output( {
influence.measures(gotelli.glm)
} )
if( length(txt) > 10 ){
    txt <- c( txt[3:13], "..." )
}
cat( txt, sep = "\n" )


###################################################
### chunk number 70: GotelliDispersion
###################################################
#via Pearson residuals
pp/gotelli.glm$df.resid
#OR via deviance
gotelli.glm$deviance/gotelli.glm$df.resid


###################################################
### chunk number 71: GotelliSummary
###################################################
summary(gotelli.glm)


###################################################
### chunk number 72: GotelliModelSelection
###################################################
library(MuMIn)
model.avg(get.models(dredge(gotelli.glm)))


###################################################
### chunk number 73: gotelli.glm
###################################################
gotelli.glm <- glm(Srich~Habitat+Latitude+Elevation, family=poisson,data=gotelli)
summary(gotelli.glm)


###################################################
### chunk number 74: gotelli
###################################################
getOption("SweaveHooks")[["fig"]]()
# Produce base plot
xs <- seq(40, 45, l = 1000)
plot(Srich ~ Latitude, data = gotelli, type = "n", axes = F, 
xlab = "", ylab = "")
# Plot the points and predicted trends
points(Srich ~ Latitude, data = gotelli, subset = Habitat == 
"Forest", pch = 16)
pred <- predict(gotelli.glm, type = "response", se = T, newdata 
= data.frame(Latitude = xs, Habitat = "Forest", Elevation = 
mean(gotelli$Elevation)))
lines(pred$fit ~ xs)
points(Srich ~ Latitude, data = gotelli, subset = Habitat == 
"Bog", pch = 21)
pred <- predict(gotelli.glm, type = "response", se = T, newdata 
= data.frame(Latitude = xs, Habitat = "Bog", Elevation = 
mean(gotelli$Elevation)))
lines(pred$fit ~ xs)
# Axes titles
mtext("Ant species richness", 2, line = 3)
axis(2, las=1)
mtext(expression(paste("Latitude (", degree*N, ")")), 1, line = 3)
axis(1)
legend("topright", legend = c("Forest", "Bog"), pch = c(16, 21), 
title = "Habitat", bty = "n")
box(bty = "l")


###################################################
### chunk number 75: SinclairReadTable
###################################################
sinclair <- read.table('sinclair.csv', header=T, sep=',')


###################################################
### chunk number 76: SinclairGLMF
###################################################
sinclair.glm <- glm(COUNT~SEX*MARROW*DEATH,family=poisson, data=sinclair)


###################################################
### chunk number 77: SinclairGLMF
###################################################
library(MuMIn)
dredge(sinclair.glm,rank="AIC")


###################################################
### chunk number 78: SinclairGLMF
###################################################
sinclair.glm <- glm(COUNT~SEX*MARROW*DEATH,family=poisson, data=sinclair)


###################################################
### chunk number 79: SinclairGLM1
###################################################
sinclair.glm1 <-update(sinclair.glm,~.-SEX:MARROW:DEATH,data=sinclair)
anova(sinclair.glm,sinclair.glm1,test="Chisq")


###################################################
### chunk number 80: SinclairConditionalInd1
###################################################
sinclair.glm2 <- update(sinclair.glm1,~.-SEX:DEATH, data=sinclair)
anova(sinclair.glm1,sinclair.glm2,test="Chisq")


###################################################
### chunk number 81: SinclairConditionalInd3
###################################################
sinclair.glm4 <- update(sinclair.glm1,~.-SEX:MARROW, data=sinclair)
anova(sinclair.glm1,sinclair.glm4,test="Chisq")


###################################################
### chunk number 82: SinclairConditionalInd2
###################################################
sinclair.glm3 <- update(sinclair.glm1,~.-DEATH:MARROW, data=sinclair)
anova(sinclair.glm1,sinclair.glm3,test="Chisq")


###################################################
### chunk number 83: sinclairPearsonResiduals
###################################################
xtabs(resid(sinclair.glm1, type="pearson") ~ SEX + MARROW + DEATH, sinclair)


###################################################
### chunk number 84: sinclairMainEffectsFemales
###################################################
# females
sinclair.glmR <- glm(COUNT ~ DEATH + MARROW, family = poisson, 
data = sinclair, subset = SEX == "FEMALE")
sinclair.glmF <- glm(COUNT ~ DEATH * MARROW, family = poisson, 
data = sinclair, subset = SEX == "FEMALE")
anova(sinclair.glmR, sinclair.glmF, test = "Chisq")


###################################################
### chunk number 85: sinclairMainEffectsMales
###################################################
# males
sinclair.glmR <- glm(COUNT ~ DEATH + MARROW, family = poisson, 
data = sinclair, subset = SEX == "MALE")
sinclair.glmF <- glm(COUNT ~ DEATH * MARROW, family = poisson, 
data = sinclair, subset = SEX == "MALE")
anova(sinclair.glmR, sinclair.glmF, test = "Chisq")


###################################################
### chunk number 86: SinclairOddsratiosMale
###################################################
# Males
library(biology)
male.tab <- xtabs(COUNT ~ DEATH + MARROW, data=sinclair, subset=
SEX == "MALE")
# transpose to express in the context of cause of death
male.tab <- t(male.tab)
oddsratios(male.tab)


###################################################
### chunk number 87: SinclairOddsratiosFemale
###################################################
# Females
female.tab <- xtabs(COUNT ~ DEATH + MARROW, data=sinclair, subset=
SEX == "FEMALE")
female.tab <- t(female.tab)
oddsratios(female.tab)


###################################################
### chunk number 88: sinclairOddsPlot
###################################################
getOption("SweaveHooks")[["fig"]]()
# make a table for females
female.tab <- xtabs(COUNT ~ DEATH + MARROW, data=sinclair, subset=SEX=="FEMALE")
# calculate the odds ratios for females
# the table should be transposed such that cause of death are in columns
library(biology)
sinclair.or<-oddsratios(t(female.tab))
plot(estimate~as.numeric(Comparison), data=sinclair.or,
log="y", type="n", axes=F, xlab="",ylab="", ylim=range(c(upper,lower)),
xlim=c(.5,3.5))
# plot the female data displaced to the right slightly
with(sinclair.or,points(as.numeric(Comparison)+.1, estimate,type="b"))
with(sinclair.or, arrows(as.numeric(Comparison)+.1, upper,
as.numeric(Comparison)+.1,lower, ang=90, length=.1, code=3))
# make the male table
male.tab <- xtabs(COUNT ~ DEATH + MARROW, data=sinclair, subset=SEX=="MALE")
sinclair.or<-oddsratios(t(male.tab))
# plot the male odds ratios
points(estimate~Comparison,data=sinclair.or, type="b", pch=16)
with(sinclair.or, arrows(as.numeric(Comparison), upper,
as.numeric(Comparison), lower, ang=90, length=.1, code=3))
abline(h=1,lty=2)
# put in axes and titles
with(sinclair.or,axis(1,at=as.numeric(Comparison), lab=Comparison))
axis(2,las=1, cex.axis=.75)
mtext("Marrow type",1, line=3)
mtext("Odds ratio of death by predation", 2, line=3)
legend("topright",legend=c("Male","Female"), pch=c(16,21), bty="n", title="Sex")
box(bty="l")


###################################################
### chunk number 89: TaulmanReadTable
###################################################
taulman <- read.table('taulman.csv', header=T,sep=',')


###################################################
### chunk number 90: TaulmanFactor
###################################################
taulman$YEAR <-  as.factor(taulman$YEAR)


###################################################
### chunk number 91: TaulmanGLM
###################################################
taulman.glm <- glm(COUNT~TREAT*YEAR*AGE, family=poisson, data=taulman)


###################################################
### chunk number 92: TaulmanDredge
###################################################
dredge(taulman.glm, rank="AIC", fixed=~TREAT:YEAR)


###################################################
### chunk number 93: TaulmanGLM1
###################################################
taulman.glm1 <- update(taulman.glm,~.-TREAT:YEAR:AGE, data=taulman)
anova(taulman.glm,taulman.glm1,test="Chisq")
AIC(taulman.glm1)-AIC(taulman.glm)


###################################################
### chunk number 94: TaulmanGLM2
###################################################
taulman.glm2 <- update(taulman.glm,~.-YEAR:AGE-TREAT:YEAR:AGE, data=taulman)
anova(taulman.glm,taulman.glm2,test="Chisq")
AIC(taulman.glm2)-AIC(taulman.glm)


###################################################
### chunk number 95: TaulmanGLM3
###################################################
taulman.glm3 <- update(taulman.glm,~.-TREAT:AGE-TREAT:YEAR:AGE, data=taulman)
anova(taulman.glm,taulman.glm3,test="Chisq")
AIC(taulman.glm3)-AIC(taulman.glm)
dredge(taulman.glm, rank="AIC", fixed=~TREAT:YEAR)


###################################################
### chunk number 96: taulmanOddsPlot
###################################################
getOption("SweaveHooks")[["fig"]]()
# make a table for control sites
control.tab <- xtabs(COUNT ~ AGE + YEAR, data=taulman,subset=TREAT=="CONTROL")
# calculate the odds ratios for control sites, corrected for 0 values
library(biology)
taulman.or <- oddsratios(t(control.tab),corr=T)
plot(estimate~as.numeric(Comparison), data=taulman.or, log="y",type="n", 
axes=F, xlab="",ylab="", ylim=range(c(upper,lower)), xlim=c(.5,3.5))
# plot the control data displaced to the right slightly
with(taulman.or, points(as.numeric(Comparison)+.1, estimate, type="b", pch=21))
with(taulman.or, arrows(as.numeric(Comparison)+.1, upper, as.numeric(Comparison)+.1, 
lower, ang=90, length=.1, code=3))
# make the logging table
harvest.tab <- xtabs(COUNT ~ AGE + YEAR, data=taulman, subset=TREAT=="HARVEST")
taulman.or<-oddsratios(t(harvest.tab),corr=T)
# plot the logging odds ratios
points(estimate~Comparison, data=taulman.or,type="b", pch=16)
with(taulman.or, arrows(as.numeric(Comparison), upper, as.numeric(Comparison), 
lower, ang=90, length=.1, code=3))
abline(h=1,lty=2)
# put in axes and titles
axis(1,at=as.numeric(taulman.or$Comparison), lab=taulman.or$Comparison)
axis(2,las=1, cex.axis=.75)
mtext("Year",1, line=3)
mtext("Odds ratio of adults", 2, line=3)
legend("topright",legend=c("Logging","Control"), pch=c(16,21), bty="n", title="Sex")
box(bty="l")


###################################################
### chunk number 97: loynGAMReadTable
###################################################
loyn <- read.table('loyn.csv', header=T, sep=',')


###################################################
### chunk number 98: loynGAMScatterPlot1
###################################################
getOption("SweaveHooks")[["fig"]]()
scatterplot.matrix(~ ABUND + AREA + I(1987-YR.ISOL) + DIST, data=loyn, diag="boxplot")


###################################################
### chunk number 99: LoynGAMVIF
###################################################
library(car)
vif(glm(ABUND~log10(AREA)+I(1987-YR.ISOL)+log10(DIST), family=gaussian,data=loyn))
#library(mgcv)
#loyn.gam<-gam(ABUND~s(log10(AREA))+s(I(1987-YR.ISOL))+s(log10(DIST)),
#family=gaussian, data=loyn)
#library(car)
#vif(loyn.gam)


###################################################
### chunk number 100: LoynGAM
###################################################
library(gam)
loyn.gam<-gam(ABUND~lo(log10(AREA))+lo(I(1987-YR.ISOL))+lo(log10(DIST)), 
family=gaussian,data=loyn)


###################################################
### chunk number 101: LoynGAMDeviance
###################################################
paste("Deviance:",format(loyn.gam$deviance))
1-pchisq(loyn.gam$deviance, loyn.gam$df.resid)


###################################################
### chunk number 102: LoynGAMResiduals
###################################################
getOption("SweaveHooks")[["fig"]]()
# extract the Pearson's residuals from the fitted gam
loyn.Res <- residuals(loyn.gam, "pearson")
#generate a data frame with all transformations
loyn.mod <- with(loyn, data.frame(ABUND, L10AREA = log10(AREA), 
YRSISOL = I(1987 - YR.ISOL), L10DIST = log10(DIST)))
# rearrange this data frame such that each of the predictors become 
# levels of a factor vector
loyn.L <- reshape(loyn.mod, direction = "long", varying = list(c(2, 
3, 4)), timevar = "Predictor", v.names = "Var", times = names(loyn.mod[, 
c(2, 3, 4)]))
# add the residuals to this data frame
loyn.L$Res <- rep(loyn.Res, 3)
# construct a lattice graphic
library(lattice)
print(xyplot(Res ~ Var | Predictor, data = loyn.L, scales = list(
alternating = TRUE, x = list(relation = "free")), xlab=
"Predictor variables", panel = function(x,y) {
 panel.points(x, y, col = 1, pch = 16)
 panel.loess(x, y, lwd = 2, col = 1)
 }
 ))


###################################################
### chunk number 103: LoynGAMSummary
###################################################
summary(loyn.gam)


###################################################
### chunk number 104: LoynGam1
###################################################
loyn.gam1<-update(loyn.gam, ~.-lo(log10(AREA)),
family=gaussian,data=loyn)
anova(loyn.gam, loyn.gam1, test="F")


###################################################
### chunk number 105: LoynGAM2
###################################################
loyn.gam2<-update(loyn.gam, ~.-lo(I(1987 - YR.ISOL)),
family=gaussian,data=loyn)
anova(loyn.gam, loyn.gam2, test="F")


###################################################
### chunk number 106: LoynGAM3
###################################################
loyn.gam3<-update(loyn.gam, ~.-lo(log10(DIST)),
family=gaussian,data=loyn)
anova(loyn.gam, loyn.gam3, test="F")


###################################################
### chunk number 107: LoynGAMModelSelection
###################################################
library(MuMIn)
dredge(loyn.gam)


###################################################
### chunk number 108: CreateRFile
###################################################
Stangle("../../GLM.rnw")


