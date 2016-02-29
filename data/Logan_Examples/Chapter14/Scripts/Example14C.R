###################################################
### chunk number 1: pg 421
###################################################
mullens <- read.table('mullens.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 421
###################################################
mullens$O2LEVEL <-factor(mullens$O2LEVEL)


###################################################
### chunk number 3: pg 421
###################################################
library(alr3)
resplot(lm(FREQBUC ~ BRTH.TYP * O2LEVEL + TOAD, data=mullens))


###################################################
### chunk number 4: pg 421
###################################################
resplot(lm(sqrt(FREQBUC) ~ BRTH.TYP * O2LEVEL + TOAD, data=mullens))


###################################################
### chunk number 5: pg 422
###################################################
library(nlme)
mullens.agg <- gsummary(mullens, groups=mullens$TOAD)
boxplot(FREQBUC ~ BRTH.TYP, mullens.agg)


###################################################
### chunk number 6: pg 422
###################################################
mullens$SFREQBUC <- sqrt(mullens$FREQBUC)
mullens.agg <- gsummary(mullens, groups=mullens$TOAD)
boxplot(SFREQBUC ~ BRTH.TYP, mullens.agg)


###################################################
### chunk number 7: pg 423
###################################################
boxplot(FREQBUC ~ O2LEVEL, mullens)


###################################################
### chunk number 8: pg 423
###################################################
boxplot(FREQBUC ~ BRTH.TYP*O2LEVEL, mullens)


###################################################
### chunk number 9: pg 423
###################################################
boxplot(sqrt(FREQBUC) ~ O2LEVEL, mullens)


###################################################
### chunk number 10: pg 423
###################################################
boxplot(sqrt(FREQBUC) ~ BRTH.TYP * O2LEVEL, mullens)


###################################################
### chunk number 11: pg 423
###################################################
replications(sqrt(FREQBUC)~BRTH.TYP*O2LEVEL+Error(TOAD),mullens)
library(biology)
is.balanced(sqrt(FREQBUC)~BRTH.TYP*O2LEVEL+Error(TOAD),mullens)


###################################################
### chunk number 12: pg 424
###################################################
contrasts(mullens$TOAD)<- contr.helmert
contrasts(mullens$BRTH.TYP)<- contr.helmert
contrasts(mullens$O2LEVEL)<- contr.poly(8,c(0,5,10,15,20,30,40,50))
# create a new variable to represent the transformed response
mullens$SFREQBUC <- sqrt(mullens$FREQBUC)
mullens.aov <- aov(SFREQBUC~BRTH.TYP*O2LEVEL + Error(TOAD),data=mullens)
library(biology)
AnovaM(mullens.aov,type="III", RM=T)


###################################################
### chunk number 13: pg 425
###################################################
library(biology)
summary(mainEffects(mullens.aov,at=BRTH.TYP=="buccal"))


###################################################
### chunk number 14: pg 426
###################################################
summary(mainEffects(mullens.aov,at=BRTH.TYP=="lung"))


###################################################
### chunk number 15: pg 426
###################################################
# begin by defining the appropriate linear, quadratic and cubic terms
p1 <- C(mullens$O2LEVEL, poly, 1, c(0, 5, 10, 15, 20, 30, 40, 50))
p2 <- C(mullens$O2LEVEL, poly, 2, c(0, 5, 10, 15, 20, 30, 40, 50))
p3 <- C(mullens$O2LEVEL, poly, 3, c(0, 5, 10, 15, 20, 30, 40, 50))
# calculate the Linear trend
# Note the use of Type III Sums of Squares due to design imbalance
mullens.aovP1 <- aov(SFREQBUC ~ BRTH.TYP * p1 + Error(TOAD/(p1)), data = mullens)
AnovaM(mullens.aovP1, type = "III")[[2]]
# calculate the Quadratic trend
mullens.aovP2 <- aov(SFREQBUC ~ BRTH.TYP * (p1 + p2) + Error(TOAD/(p1 + p2)),
data = mullens)
AnovaM(mullens.aovP2, type = "III")[[3]]
# calculate the Cubic trend
mullens.aovP3 <- aov(SFREQBUC ~ BRTH.TYP * (p1 + p2 + p3) + Error(TOAD/(p1 + p2 + p3)), data = mullens)
AnovaM(mullens.aovP3, type = "III")[[4]]


###################################################
### chunk number 16: pg 427
###################################################
# explore the trends for buccal breathing toads
library(biology)
mullens.aovB <- aov(SFREQBUC~p1+p2+p3 + Error(TOAD/(p1+p2+p3)), data=mullens, subset=BRTH.TYP=="buccal")
AnovaM(mullens.aovB)


###################################################
### chunk number 17: pg 427
###################################################
# explore the trends for lung breathing toads}
mullens.aovL <- aov(SFREQBUC~p1+p2+p3 + Error(TOAD/(p1+p2+p3)), data=mullens, subset=BRTH.TYP=="lung")
AnovaM(mullens.aovL)


###################################################
### chunk number 18: pg 428
###################################################
# calculate the mean and standard error of each group
mullens.means<-with(mullens, tapply(SFREQBUC,list(BRTH.TYP,O2LEVEL),mean))
mullens.se<-with(mullens, tapply(SFREQBUC,list(BRTH.TYP,O2LEVEL), function(x) ci(x)[4]))
mullens$O2 <- as.numeric(as.character(mullens$O2LEVEL))
# create a numeric version of the oxygen level variable
xval <- unique(mullens$O2)
# construct the base plot
plot(SFREQBUC~O2, data=mullens, type="n", axes=F, xlab="",ylab="")
# create some shortcuts objects
mB<-mullens.means["buccal",]
seB<-mullens.se["buccal",]
# plot the error bars with open circle symbols for buccal breathing toads
arrows(xval,mB-seB,xval,mB+seB, code=3,angle=90, len=.01)
points(mB~xval, pch=16,col="white", type="b", lwd=1, lty=2)
points(mB~xval, pch=1,col="black", type="b", lwd=1, lty=2)
mL<-mullens.means["lung",]
seL<-mullens.se["lung",]
points(mL~xval, pch=19, type="b", lwd=1, lty=1)
# plot the error bars with open circle symbols for lung breathing toads
arrows(xval,mL-seL,xval,mL+seL, code=3,angle=90, len=.01)
# construct axes
axis(1,cex.axis=.8)
mtext(text=expression(paste(O[2], " level (%)")), side=1, line=3)
axis(2, cex.axis=.8, las=1)
mtext(text=expression(paste("Breathing rate ", (sqrt(breaths.m^{-1})))), side=2, line=3)
# add a legend
legend("topright",leg=c("buccal","lung"), title="Breathing type", lty=0, pch=c(22,19), bty="n", cex=1)
box(bty="l")
