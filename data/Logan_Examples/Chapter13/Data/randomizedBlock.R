###################################################
### chunk number 1: pg 388
###################################################
driscoll1 <- driscoll
driscoll1[9,4] <- NA


###################################################
### chunk number 2: pg 388
###################################################
driscoll1.aov <- aov(CALLS~Error(BLOCK)+YEAR, data=driscoll1, subset=BLOCK!="newpipe")
summary(driscoll1.aov)


###################################################
### chunk number 3: pg 389
###################################################
#calculate the mean of the newpipe block
BM<-with(driscoll1, tapply(CALLS, BLOCK, mean, na.rm=T))["newpipe"]
#calculate the mean of year 2
YM<-with(driscoll1, tapply(CALLS, YEAR, mean, na.rm=T))["2"]
#calculate the overall mean
M<-mean(driscoll1$CALLS,na.rm=T)
#duplicate the data set and work on the duplicate
driscoll2 <- driscoll1
#substitute the new value into the data frame
driscoll2[9,3]<-YM+BM-M
#fit the linear model
driscoll2.aov <- aov(CALLS~Error(BLOCK)+YEAR, data=driscoll2)
summary(driscoll2.aov)
#then make adjustments to the F-ratio and Pvalue (to reflect a reduction) 
#in residual degrees of freedom by one for each substituted value)
(MSresid <- summary(driscoll2.aov)[[2]][[1]]["Residuals","Sum Sq"]/9)
(Fyear <- summary(driscoll2.aov)[[2]][[1]]["YEAR","Mean Sq"]/MSresid)
(Pvalue <- 1-pf(Fyear, 2,8))


###################################################
### chunk number 4: pg 389
###################################################
#fit full model
driscoll1.aovF <- aov(CALLS~BLOCK+YEAR, data=driscoll1)
#fit reduced model
driscoll1.aovR <- aov(CALLS~BLOCK, data=driscoll1)
anova(driscoll1.aovF,driscoll1.aovR)


###################################################
### chunk number 5: pg 390
###################################################
anova(driscoll1.aovF)


###################################################
### chunk number 6: pg 390
###################################################
library(nlme)
#No structure
driscoll1.lme1 <- lme(CALLS ~ YEAR, random = ~1 | BLOCK, data = driscoll1, subset = !is.na(CALLS))
#Unstructured 
driscoll1.lme2 <- lme(CALLS ~ YEAR, random = ~1 | BLOCK, data = driscoll1, subset = !is.na(CALLS), correlation = corSymm(form = ~1 | BLOCK))
#Compound symmetry
driscoll1.lme3 <- update(driscoll1.lme1, correlation = corCompSymm(form = ~1 | BLOCK))
#First order autoregressive
driscoll1.lme4 <- lme(CALLS ~ YEAR, random = ~1 | BLOCK, data = driscoll1, subset = !is.na(CALLS), correlation = corAR1(form = ~1 | BLOCK))
driscoll1.lme4 <- update(driscoll1.lme1, correlation = corAR1(form = ~1 | BLOCK))
#Compare each to compound symmetry
#technically,only models fitted with ML (not REML)
#should be compared via anova
driscoll1.lmeML1<-update(driscoll1.lme1, method="ML")
driscoll1.lmeML2<-update(driscoll1.lme2, method="ML")
driscoll1.lmeML3<-update(driscoll1.lme3, method="ML")
driscoll1.lmeML4<-update(driscoll1.lme4, method="ML")
anova(driscoll1.lme3, driscoll1.lme1, driscoll1.lme2, driscoll1.lme4)
anova(driscoll1.lme3)


###################################################
### chunk number 74: CropReadTable
###################################################
crop <- read.table('crop.csv', header=T, sep=',')


###################################################
### chunk number 75: CropFactor
###################################################
crop$A <-factor(crop$A)
crop$B <-factor(crop$B)
crop$S <-factor(crop$S)


###################################################
### chunk number 76: Options
###################################################
op <- options(width=40)


###################################################
### chunk number 77: cropStdResid1
###################################################
library(alr3)
resplot(lm(Y~S+A*B, crop))


###################################################
### chunk number 78: cropInteractionPlot1
###################################################
with(crop,interaction.plot(S,paste("A",A,":B",B, 
sep=""),Y))


###################################################
### chunk number 79: Options
###################################################
options(op)


###################################################
### chunk number 80: cropBoxplot1
###################################################
boxplot(Y~A*B, crop)


###################################################
### chunk number 81: CropReplications
###################################################
replications(Y~Error(S)+A*B, data=crop)
library(biology)
is.balanced(Y~Error(S)+A*B, data=crop)


###################################################
### chunk number 82: CropAov
###################################################
crop.aov <- aov(Y~Error(S)+A*B, data=crop)


###################################################
### chunk number 83: CropAov1 eval=FALSE
###################################################
## crop.aov <- aov(Y~A*B+Error(S/A + S/B), data=crop)


###################################################
### chunk number 84: CropSummary
###################################################
summary(crop.aov)


###################################################
### chunk number 85: CropMainEffects
###################################################
#Examine the effects of B at A=1
summary(mainEffects(crop.aov, at = A == "1"))
#Examine the effects of B at A=2
summary(mainEffects(crop.aov, at = A == "2"))
#Examine the effects of B at A=3
summary(mainEffects(crop.aov, at = A == "3"))


###################################################
### chunk number 86: crop1
###################################################
# calculate the mean and se of each A:B combination
crop.means <- with(crop, t(tapply(Y, list(A,B), mean)))
library(gmodels)
crop.se<-with(crop, t(tapply(Y,list(A,B), function(x) ci(x, na.rm=T)[4])))
# produce the base plot with bars offset down to meet the axis
ofst <- min(crop$Y)
xs <- barplot(crop.means,ylim=range(crop$Y, na.rm=T), beside=T, axes=F, xpd=T, 
axisnames=F, axis.lty=2, legend.text=F, col=c(0,1), offset=ofst)
# plot the error bars
arrows(xs,crop.means+ofst,xs,crop.means+crop.se+ofst, code=2, angle=90, len=.05)
# create the axes and their labels
axis(2,las=1)
axis(1,at=apply(xs,2,median), lab=c("A1", "A2", "A3"), padj=1, mgp=c(0,0,0))
mtext(2,text=expression(paste("Crop yield ",(g.mˆ2))),line=3, cex=1)
mtext(1,text="Sewing density",line=3, cex=1)
box(bty="l", xpd=1)
# include a legend
legend("topleft",leg=c("B1", "B2"), fill=c(0,1), col=c(0,1),bty="n", cex=1)


###################################################
### chunk number 87: GPReadTable
###################################################
gp <- read.table('gp.csv', header=T, sep=',')


###################################################
### chunk number 88: gpStdResid1
###################################################
library(alr3)
resplot(lm(GAIN~BLOCK+DIET, gp))


###################################################
### chunk number 89: gpInteractionPlot1
###################################################
with(gp,interaction.plot(BLOCK,DIET,GAIN))


###################################################
### chunk number 90: gpBoxplot1
###################################################
boxplot(GAIN~DIET, gp)


###################################################
### chunk number 91: GPReplications
###################################################
replications(GAIN~Error(BLOCK)+DIET, data=gp)
library(biology)
is.balanced(GAIN~Error(BLOCK)+DIET, data=gp)


###################################################
### chunk number 92: gpFriedman
###################################################
library(pgirmess)
friedman.test(GAIN~DIET|BLOCK, data=gp)


###################################################
### chunk number 93: Friedmanmc
###################################################
library(pgirmess)
friedmanmc(gp$GAIN,gp$DIET,gp$BLOCK, p=0.05)


###################################################
### chunk number 94: GPRank
###################################################
summary(aov(rank(GAIN)~Error(BLOCK)+DIET, gp))


###################################################
### chunk number 95: GLTukeys
###################################################
library(multcomp)
summary(glht(aov(rank(GAIN)~BLOCK+DIET, gp), linfct=mcp(DIET="Tukey")))


###################################################
### chunk number 96: gp1
###################################################
# calculate the mean and se of each A:B combination
gp.means <- with(gp, t(tapply(GAIN, DIET, mean)))
library(gmodels)
#calculate the residuals from the model
gp.res<-resid(aov(GAIN~BLOCK+DIET, gp))
#calculate the standard error in the residuals
gp.se<-with(gp, t(tapply(gp.res,DIET, function(x) ci(x, na.rm=T)[4])))
# produce the base plot
xs <- barplot(gp.means, ylim=range(gp$GAIN),beside=T, axes=F, xpd=F,axisnames=F, 
axis.lty=2, legend.text=F, col=c(0,0))
# plot the error bars
arrows(xs,gp.means+ofst,xs,gp.means+gp.se, code=2, angle=90, len=.05)
# create the axes and their labels
axis(2,las=1)
axis(1,at=apply(xs,2,median), lab=c("D1", "D2", "D3", "D4"), padj=1, mgp=c(0,0,0))
mtext(2,text=expression(paste("Weight gain ",(g))),line=3, cex=1)
mtext(1,text="Diet",line=3, cex=1)
box(bty="l", xpd=1)
# include Tukey's multiple comparisons summaries
text(gp.means+gp.se+.5~xs, lab=c("A","B","B","A"))


###################################################
### chunk number 97: CreateRFile
###################################################
Stangle("../../randomizedBlock.rnw")


