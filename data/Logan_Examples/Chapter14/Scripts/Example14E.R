###################################################
### chunk number 1: pg 434
###################################################
mcgold <- read.table('mcgold.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 434
###################################################
# examine the first six entries in the MONTH vector
head(mcgold$MONTH)
mcgold$MONTH <- ordered(mcgold$MONTH, levels = unique(mcgold$MONTH))
# examine the first six entries in the MONTH vector again
# note the format of the levels attribute
head(mcgold$MONTH)


###################################################
### chunk number 3: pg 434
###################################################
library(alr3)
resplot(lm(BIRDS ~ HABITAT * REGION * MONTH + SITE, data=mcgold))


###################################################
### chunk number 4: pg 434
###################################################
resplot(lm(log(BIRDS+1) ~ HABITAT * REGION * MONTH + SITE, data=mcgold))


###################################################
### chunk number 5: pg 435
###################################################
library(nlme)
mcgold$LBIRDS <- log(mcgold$BIRDS+1)
mcgold.agg <- gsummary(mcgold, groups=mcgold$SITE)
boxplot(BIRDS~HABITAT*REGION, mcgold.agg)


###################################################
### chunk number 6: pg 435
###################################################
boxplot(LBIRDS~HABITAT*REGION, mcgold.agg)


###################################################
### chunk number 7: pg 435
###################################################
boxplot(BIRDS~MONTH,mcgold)


###################################################
### chunk number 8: pg 435
###################################################
boxplot(LBIRDS~MONTH,mcgold)


###################################################
### chunk number 9: pg 436
###################################################
replications(log(BIRDS+1)~HABITAT*REGION*MONTH+Error(SITE), mcgold)
library(biology)
is.balanced(log(BIRDS+1)~HABITAT*REGION*MONTH+Error(SITE), mcgold)


###################################################
### chunk number 10: pg 436
###################################################
mcgold.aov<-aov(LBIRDS~HABITAT*REGION*MONTH+Error(SITE), data=mcgold)
library(biology)
AnovaM(mcgold.aov, RM=T)


###################################################
### chunk number 11: pg 438
###################################################
# begin by defining the appropriate linear, quadratic and cubic terms
MONTH.L <- C(mcgold$MONTH, poly, 1) # linear trend
MONTH.Q <- C(mcgold$MONTH, poly, 2) # quadratic trend
MONTH.C <- C(mcgold$MONTH, poly, 3) # cubic trend
mcgold.aov <- aov(LBIRDS ~ HABITAT * REGION * MONTH + Error(SITE/(MONTH.L +
MONTH.Q + MONTH.C)), data = mcgold)
summary(mcgold.aov)


###################################################
### chunk number 12: pg 439
###################################################
library(biology)
summary(mainEffects(mcgold.aov, at=HABITAT=="ironbark"))


###################################################
### chunk number 13: pg 440
###################################################
library(biology)
summary(mainEffects(mcgold.aov, at=HABITAT=="stringybark"))


###################################################
### chunk number 14: pg 441
###################################################
# calculate the mean and standard error of each group
mcgold.means <- with(mcgold, tapply(BIRDS + 1, list(interaction(HABITAT, REGION), MONTH), mean))
library(gmodels)
mcgold.se <- with(mcgold, tapply(BIRDS + 1, list(interaction(HABITAT, REGION), MONTH), function(x) ci(x)[4]))
xval <- as.numeric(mcgold$MONTH)
# construct the base plot
plot(BIRDS~xval, data=mcgold, type="n", axes=F, xlab="",ylab="", log="y")
xval<-unique(xval)
points(mcgold.means["ironbark.north",]~xval, pch=1,col="black", type="b", lwd=1, lty=1)
points(mcgold.means["ironbark.south",]~xval,pch=16,col="black", type="b", lwd=1, lty=2)
points(mcgold.means["stringybark.north",]~xval, pch=2,col="black", type="b", lwd=1, lty=1)
points(mcgold.means["stringybark.south",]~xval, pch=17,col="black", type="b", lwd=1, lty=2)
# plot axes
axis(1,cex.axis=.8, at=xval, lab=substr(levels(mcgold$MONTH), 1, 1))
mtext(text="Month", side=1, line=3)
# define new tick marks
yticks <- ifelse(axTicks(2)>9,axTicks(2),axTicks(2)-1)
axis(2, cex.axis=.8, las=1, at=axTicks(2),lab=yticks)
mtext(text="Mean number of birds", side=2, line=3)
# add a legend
legend("topright",leg=c("Ironbark north","Ironbark South", "Stringybark north", "Stringybark south"), lty=0,pch=c(1,16,2,17), bty="n", cex=1)
box(bty="l")

