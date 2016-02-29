###################################################
### chunk number 1: pg 391
###################################################
crop <- read.table('crop.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 391
###################################################
crop$A <-factor(crop$A)
crop$B <-factor(crop$B)
crop$S <-factor(crop$S)


###################################################
### chunk number 3: pg 391
###################################################
library(alr3)
resplot(lm(Y~S+A*B, crop))


###################################################
### chunk number 4: pg 391
###################################################
with(crop,interaction.plot(S,paste("A",A,":B",B, sep=""),Y))


###################################################
### chunk number 5: pg 392
###################################################
boxplot(Y~A*B, crop)


###################################################
### chunk number 6: pg 392
###################################################
replications(Y~Error(S)+A*B, data=crop)
library(biology)
is.balanced(Y~Error(S)+A*B, data=crop)


###################################################
### chunk number 7: pg 392
###################################################
#Additive model
crop.aov <- aov(Y~Error(S)+A*B, data=crop)
#Non-additive model
#crop.aov <- aov(Y~A*B+Error(S/A + S/B), data=crop)


###################################################
### chunk number 8: pg 392
###################################################
summary(crop.aov)


###################################################
### chunk number 9: pg 393
###################################################
#Examine the effects of B at A=1
summary(mainEffects(crop.aov, at = A == "1"))
#Examine the effects of B at A=2
summary(mainEffects(crop.aov, at = A == "2"))
#Examine the effects of B at A=3
summary(mainEffects(crop.aov, at = A == "3"))


###################################################
### chunk number 10: pg 394
###################################################
# calculate the mean and se of each A:B combination
crop.means <- with(crop, t(tapply(Y, list(A,B), mean)))
library(gmodels)
crop.se<-with(crop, t(tapply(Y,list(A,B), function(x) ci(x, na.rm=T)[4])))
# produce the base plot with bars offset down to meet the axis
ofst <- min(crop$Y)
xs <- barplot(crop.means,ylim=range(crop$Y, na.rm=T), beside=T, axes=F, xpd=T, axisnames=F, axis.lty=2, legend.text=F, col=c(0,1), offset=ofst)
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
