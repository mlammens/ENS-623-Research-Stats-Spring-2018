###################################################
### chunk number 1: pg 376
###################################################
walter <- read.table('walter.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 376
###################################################
walter$BLOCK <-factor(walter$BLOCK)
class(walter$BLOCK)


###################################################
### chunk number 3: pg 377
###################################################
boxplot(MITE~TREAT, walter)


###################################################
### chunk number 4: pg 377
###################################################
boxplot(log(0.5+(MITE*10))~TREAT, walter)


###################################################
### chunk number 5: pg 377
###################################################
library(alr3)
resplot(lm(MITE~BLOCK+TREAT, walter))


###################################################
### chunk number 6: pg 377
###################################################
with(walter,interaction.plot(BLOCK,TREAT,MITE))


###################################################
### chunk number 7: pg 378
###################################################
library(alr3)
resplot(lm(log(0.5+(MITE*10))~BLOCK+TREAT, walter))


###################################################
### chunk number 8: pg 378
###################################################
with(walter,interaction.plot(BLOCK,TREAT, log(0.5+(MITE*10))))


###################################################
### chunk number 9: pg 378
###################################################
replications(log(0.5+(MITE*10))~Error(BLOCK)+TREAT, data=walter)
library(biology)
is.balanced(log(0.5+(MITE*10))~Error(BLOCK)+TREAT, data=walter)


###################################################
### chunk number 10: pg 378
###################################################
walter.aov <- aov(log(0.5+(MITE*10))~Error(BLOCK)+TREAT, data=walter)


###################################################
### chunk number 11: pg 378
###################################################
summary(walter.aov)


###################################################
### chunk number 12: pg 379
###################################################
op<-par(mar=c(4,4,.1,.1))
plot(MITE~as.numeric(BLOCK), data=walter, type="n", axes=F, xlab="",ylab="")
with(subset(walter, TREAT=="Without.domatia"),points(MITE~as.numeric(BLOCK), pch=21, type="o", lwd=1))
with(subset(walter, TREAT=="With.domatia"),points(MITE~as.numeric(BLOCK), pch=16, type="o", lwd=1, lty=1))
axis(1,cex.axis=.8)
mtext(text="Leaf pair", side=1, line=3)
axis(2, cex.axis=.8, las=1)
mtext(text="Number of mites per leaf", side=2, line=3)
legend("topright",leg=c("Without domatia","With domatia"), lty=0,pch=c(21,16), bty="n", cex=.7)
box(bty="l")
par(op)
