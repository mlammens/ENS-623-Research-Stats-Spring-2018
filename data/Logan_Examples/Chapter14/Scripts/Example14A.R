###################################################
### chunk number 1: pg 413
###################################################
spf <- read.table('spf.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 413
###################################################
library(alr3)
resplot(lm(Y~A*C+B, data=spf))


###################################################
### chunk number 3: pg 413
###################################################
with(spf,interaction.plot(spf$B, spf$C,spf$Y))


###################################################
### chunk number 4: pg 414
###################################################
library(nlme)
spf.agg <- gsummary(spf, groups=spf$B)
boxplot(Y~A, spf.agg)


###################################################
### chunk number 5: pg 414
###################################################
boxplot(Y~C, spf)


###################################################
### chunk number 6: pg 414
###################################################
boxplot(Y~A*C, spf)


###################################################
### chunk number 7: pg 415
###################################################
replications(Y~A*C+Error(B), data=spf)
library(biology)
is.balanced(Y~A*C+Error(B), data=spf)


###################################################
### chunk number 8: pg 415
###################################################
spf.aov <- aov(Y~A*C+Error(B), spf)
summary(spf.aov)


###################################################
### chunk number 9: pg 415
###################################################
library(biology)
summary(mainEffects(spf.aov,at=A=="A1"))


###################################################
### chunk number 10: pg 416
###################################################
library(biology)
summary(mainEffects(spf.aov,at=A=="A2"))


###################################################
### chunk number 11: pg 416
###################################################
#fit a fully factorial anova to obtain a pooled error term
spf.aovA <- aov(Y~A*C,spf)
summary(mainEffects(spf.aovA,at=C=="C1"))


###################################################
### chunk number 12: pg 416
###################################################
summary(mainEffects(spf.aovA,at=C=="C2"))


###################################################
### chunk number 13: pg 417
###################################################
# define each of the separate polynomial terms
p1<-C(spf$C, poly,1)
p2<-C(spf$C, poly,2)
p3<-C(spf$C, poly,3)
spf.aov <- aov(Y~A*(p1+p2+p3)+Error(B/(p1+p2+p3)),spf)
summary(spf.aov)


###################################################
### chunk number 14: pg 418
###################################################
getOption("SweaveHooks")[["fig"]]()
# calculate the mean and standard error of each group
spf.means<-with(spf, tapply(Y,list(A,C),mean))
library(gmodels)
spf.se<-with(spf,tapply(Y,list(A,C), function(x) ci(x)[4]))
# construct the base plot with different point types for each treatment
plot(Y~as.numeric(C), data=spf, type="n", axes=F, xlab="",ylab="")
xval<-as.numeric(spf$C)
points(spf.means["A1",], pch=22, type="b", lwd=1, lty=2)
# apply error bars
arrows(xval,spf.means["A1",]-spf.se["A1",],xval, spf.means["A1",]+spf.se["A1",], code=3,angle=90, len=.05)
points(spf.means["A2",], pch=19, type="b", lwd=1, lty=1)
arrows(xval,spf.means["A2",]-spf.se["A2",],xval, spf.means["A2",]+spf.se["A2",], code=3,angle=90, len=.05)
# create the axes and their labels
axis(1,at=1:4,cex.axis=.8)
mtext(text="Time period (h)", side=1, line=3)
axis(2, cex.axis=.8, las=1)
mtext(text="Response latency", side=2, line=3)
# include a legend
legend("topleft",leg=c("Visual","Auditory"), lty=0,pch=c(22,19), 
bty="n", cex=1)
box(bty="l")
