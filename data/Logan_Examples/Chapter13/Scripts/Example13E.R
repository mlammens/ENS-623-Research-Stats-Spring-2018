###################################################
### chunk number 1: pg 395
###################################################
gp <- read.table('gp.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 395
###################################################
library(alr3)
resplot(lm(GAIN~BLOCK+DIET, gp))


###################################################
### chunk number 3: pg 395
###################################################
with(gp,interaction.plot(BLOCK,DIET,GAIN))


###################################################
### chunk number 4: pg 395
###################################################
boxplot(GAIN~DIET, gp)


###################################################
### chunk number 5: pg 396
###################################################
replications(GAIN~Error(BLOCK)+DIET, data=gp)
library(biology)
is.balanced(GAIN~Error(BLOCK)+DIET, data=gp)


###################################################
### chunk number 6: pg 396
###################################################
library(pgirmess)
friedman.test(GAIN~DIET|BLOCK, data=gp)


###################################################
### chunk number 7: pg 396
###################################################
library(pgirmess)
friedmanmc(gp$GAIN,gp$DIET,gp$BLOCK, p=0.05)


###################################################
### chunk number 8: pg 397
###################################################
summary(aov(rank(GAIN)~Error(BLOCK)+DIET, gp))


###################################################
### chunk number 9: pg 397
###################################################
library(multcomp)
summary(glht(aov(rank(GAIN)~BLOCK+DIET, gp), linfct=mcp(DIET="Tukey")))


###################################################
### chunk number 10: pg 397
###################################################
# calculate the mean and se of each A:B combination
gp.means <- with(gp, t(tapply(GAIN, DIET, mean)))
library(gmodels)
#calculate the residuals from the model
gp.res<-resid(aov(GAIN~BLOCK+DIET, gp))
#calculate the standard error in the residuals
gp.se<-with(gp, t(tapply(gp.res,DIET, function(x) ci(x, na.rm=T)[4])))
# produce the base plot
xs <- barplot(gp.means, ylim=range(gp$GAIN),beside=T, axes=F, xpd=F,axisnames=F, axis.lty=2, legend.text=F, col=c(0,0))
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
