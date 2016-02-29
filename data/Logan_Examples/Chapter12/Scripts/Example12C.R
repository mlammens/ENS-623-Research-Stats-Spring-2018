###################################################
### chunk number 1: pg 342
###################################################
minch <- read.table('minch.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 342
###################################################
library(nlme)
minch.agg<-gsummary(minch, groups=minch$ZONE:minch$SITE)
boxplot(LIMPT100~ZONE, minch.agg)


###################################################
### chunk number 3: pg 342
###################################################
boxplot(LIMPT100~SITE, minch)


###################################################
### chunk number 4: pg 342
###################################################
boxplot(LIMPT100~ZONE*SITE, minch)


###################################################
### chunk number 5: pg 343
###################################################
boxplot(sqrt(LIMPT100)~ZONE, minch.agg)


###################################################
### chunk number 6: pg 343
###################################################
boxplot(sqrt(LIMPT100)~SITE, minch)


###################################################
### chunk number 7: pg 343
###################################################
boxplot(sqrt(LIMPT100)~ZONE*SITE, minch)



###################################################
### chunk number 8: pg 343
###################################################
replications(sqrt(LIMPT100)~ZONE*SITE, minch)
library(biology)
is.balanced(sqrt(LIMPT100)~ZONE*SITE, minch)


###################################################
### chunk number 9: pg 343
###################################################
contrasts(minch$ZONE) <- cbind(c(1/3,1/3,-1,1/3))


###################################################
### chunk number 10: pg 343
###################################################
minch.aov <- aov(sqrt(LIMPT100)~ZONE*SITE,data=minch)


###################################################
### chunk number 11: pg 344
###################################################
plot(minch.aov, which=1)



###################################################
### chunk number 12: pg 344
###################################################
library(biology)
(minch.anova<-AnovaM(minch.aov, split = list(ZONE = list('Treed vs No trees' = 1)), denoms = c("ZONE:SITE","Resid","Resid")))



###################################################
### chunk number 13: pg 345
###################################################
library(lme4)
lmer(sqrt(LIMPT100)~1+(1|ZONE)+(1|SITE)+(1|ZONE:SITE), minch)


###################################################
### chunk number 14: pg 346
###################################################
# calculate mean and se of each combination
library(gmodels)
minch.means<-t(tapply(sqrt(minch$LIMPT100),list(minch$ZONE,minch$SITE),mean))
minch.se<-t(tapply(sqrt(minch$LIMPT100),list(minch$ZONE,minch$SITE), function(x) ci(x)[4]))  
xs <- barplot(minch.means, ylim=range(sqrt(minch$LIMPT100)), beside=T, axes=F, xpd=F, axisnames=F, axis.lty=2, legend.text=F, col=c(0,1))	 
# plot the error bars of the winter-spring season
arrows(xs,minch.means,xs,minch.means+minch.se, code=3, angle=90, len=.05)
# create the axes and their labels
axis(2,las=1)
axis(1,at=apply(xs,2,median), lab=c("Land","Mid","Sea\n(-trees)","Sea\n(+trees)"), padj=1, mgp=c(0,0,0))
mtext(2,text=expression(paste(sqrt("number of limpets (x100)"))),line=3, cex=1)
mtext(1,text="Zone",line=3, cex=1)
# include a legend
legend("topright",leg=c("Site A", "Site B"), fill=c(0,1), col=c(0,1), bty="n", cex=1)
box(bty="l")
