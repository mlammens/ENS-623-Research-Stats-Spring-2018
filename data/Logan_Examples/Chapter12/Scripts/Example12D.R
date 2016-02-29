###################################################
### chunk number 1: pg 346
###################################################
reich <- read.table('reich.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 347
###################################################
boxplot(LEAFAREA~LOCATION * FUNCTION, na.omit(reich))


###################################################
### chunk number 3: pg 347
###################################################
replications(LEAFAREA~LOCATION*FUNCTION, reich)
library(biology)
is.balanced(LEAFAREA~LOCATION*FUNCTION, reich)


###################################################
### chunk number 4: pg 348
###################################################
contrasts(reich$LOCATION) <- contr.helmert
contrasts(reich$FUNCTION) <- contr.helmert


###################################################
### chunk number 5: pg 348
###################################################
reich.aov <- aov(LEAFAREA~LOCATION*FUNCTION,data=reich)


###################################################
### chunk number 6: pg 348
###################################################
plot(reich.aov, which=1)


###################################################
### chunk number 7: pg 348
###################################################
library(biology)
(reich.anova<-AnovaM(reich.aov,type="III"))


###################################################
### chunk number 8: pg 349
###################################################
AnovaM(reich.aov.shrub<-mainEffects(reich.aov, at=FUNCTION=="Shrub"), type="III")
library(multcomp)
#Tukey's post-hoc multiple comparisons test
summary(glht(reich.aov.shrub, linfct = mcp(LOCATION = "Tukey")))
#OR as confidence intervals
confint(glht(reich.aov.shrub, linfct = mcp(LOCATION = "Tukey"))) 


###################################################
### chunk number 9: pg 350
###################################################
AnovaM(reich.aov.tree<-mainEffects(reich.aov, at=FUNCTION=="Tree"), type="III")
library(multcomp)
#Tukey's post-hoc multiple comparisons test
summary(glht(reich.aov.tree, linfct = mcp(LOCATION = "Tukey")))
#OR as confidence intervals
confint(glht(reich.aov.tree, linfct = mcp(LOCATION = "Tukey"))) 


###################################################
### chunk number 10: pg 351
###################################################
# calculate mean and se of each combination
library(gmodels)
reich.means<-t(tapply(reich$LEAFAREA,list(reich$LOCATION,reich$FUNCTION), mean, na.rm=T))
reich.se<-t(tapply(reich$LEAFAREA,list(reich$LOCATION,reich$FUNCTION), function(x) ci(x, na.rm=T)[4]))
xs <- barplot(reich.means,ylim=range(reich$LEAFAREA, na.rm=T),beside=T, axes=F,xpd=F,axisnames=F,axis.lty=2,legend.text=F, col=c(0,1))
# plot the error bars
arrows(xs,reich.means,xs,reich.means+reich.se, code=2, angle=90, len=.05)
# create the axes and their labels
axis(2,las=1)
axis(1,at=apply(xs,2,median), lab=c("Newmax", "Scarolin", "Venezuel", "Wiscons"), padj=1, mgp=c(0,0,0))
mtext(2,text=expression(paste("Mean specific leaf area ", (mm^2))), line=3, cex=1)
mtext(1,text="Location",line=3, cex=1)
box(bty="l")
# include a legend
legend("topright",leg=c("Shrub", "Tree"), fill=c(0,1), col=c(0,1), bty="n", cex=1)

