###################################################
### chunk number 1: pg 244
###################################################
mytilus <- read.table('mytilus.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 244
###################################################
library(car)
scatterplot(asin(sqrt(LAP))*180/pi ~DIST, data=mytilus)


###################################################
### chunk number 3: pg 244
###################################################
plot(lm(asin(sqrt(LAP))*180/pi ~DIST, data=mytilus), which=1)


###################################################
### chunk number 4: pg 245
###################################################
mytilus.lm5<- lm(asin(sqrt(LAP))*180/pi ~ DIST + I(DIST^2) +I(DIST^3) + I(DIST^4) + I(DIST^5), mytilus)


###################################################
### chunk number 5: pg 245
###################################################
plot(mytilus.lm5, which=1)


###################################################
### chunk number 6: pg 245
###################################################
anova(mytilus.lm5)


###################################################
### chunk number 7: pg 246
###################################################
mytilus.lm1<- lm(asin(sqrt(LAP))*180/pi ~DIST, mytilus)
mytilus.lm2<- lm(asin(sqrt(LAP))*180/pi ~DIST+I(DIST^2), mytilus)
anova(mytilus.lm2, mytilus.lm1)


###################################################
### chunk number 8: pg 246
###################################################
mytilus.lm3<- lm(asin(sqrt(LAP))*180/pi ~DIST+I(DIST^2)+I(DIST^3), mytilus)
anova(mytilus.lm3, mytilus.lm2)


###################################################
### chunk number 9: pg 247
###################################################
summary(mytilus.lm3)


###################################################
### chunk number 10: pg 247
###################################################
plot(asin(sqrt(LAP))*180/pi ~ DIST, data=mytilus,pch=16, axes=F, xlab="", ylab="")
axis(1, cex.axis=.8)
mtext(text=expression(paste("Miles east of Southport, Connecticut")), side=1, line=3)
axis(2, las=1)
mtext(text=expression(paste("Arcsin ",sqrt(paste("freq. of allele ", italic("Lap"))^{94}))), side=2, line=3)
x<-seq(0,80,l=1000)
points(x,predict(mytilus.lm3, data.frame(DIST=x)), type="l")
box(bty="l")
