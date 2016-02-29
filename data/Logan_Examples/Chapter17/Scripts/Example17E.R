###################################################
### chunk number 1: pg 521
###################################################
taulman <- read.table('taulman.csv', header=T,sep=',')


###################################################
### chunk number 2: pg 521
###################################################
taulman$YEAR <-  as.factor(taulman$YEAR)


###################################################
### chunk number 3: pg 522
###################################################
taulman.glm <- glm(COUNT~TREAT*YEAR*AGE, family=poisson, data=taulman)


###################################################
### chunk number 4: pg 522
###################################################
dredge(taulman.glm, rank="AIC", fixed=~TREAT:YEAR)


###################################################
### chunk number 5: pg 522
###################################################
taulman.glm1 <- update(taulman.glm,~.-TREAT:YEAR:AGE, data=taulman)
anova(taulman.glm,taulman.glm1,test="Chisq")
AIC(taulman.glm1)-AIC(taulman.glm)


###################################################
### chunk number 6: pg 523
###################################################
taulman.glm2 <- update(taulman.glm,~.-YEAR:AGE-TREAT:YEAR:AGE, data=taulman)
anova(taulman.glm,taulman.glm2,test="Chisq")
AIC(taulman.glm2)-AIC(taulman.glm)


###################################################
### chunk number 7: pg 523
###################################################
taulman.glm3 <- update(taulman.glm,~.-TREAT:AGE-TREAT:YEAR:AGE, data=taulman)
anova(taulman.glm,taulman.glm3,test="Chisq")
AIC(taulman.glm3)-AIC(taulman.glm)
dredge(taulman.glm, rank="AIC", fixed=~TREAT:YEAR)


###################################################
### chunk number 8: pg 524
###################################################
# make a table for control sites
control.tab <- xtabs(COUNT ~ AGE + YEAR, data=taulman,subset=TREAT=="CONTROL")
# calculate the odds ratios for control sites, corrected for 0 values
library(biology)
taulman.or <- oddsratios(t(control.tab),corr=T)
plot(estimate~as.numeric(Comparison), data=taulman.or, log="y",type="n", axes=F, xlab="",ylab="", ylim=range(c(upper,lower)), xlim=c(.5,3.5))
# plot the control data displaced to the right slightly
with(taulman.or, points(as.numeric(Comparison)+.1, estimate, type="b", pch=21))
with(taulman.or, arrows(as.numeric(Comparison)+.1, upper, as.numeric(Comparison)+.1, lower, ang=90, length=.1, code=3))
# make the logging table
harvest.tab <- xtabs(COUNT ~ AGE + YEAR, data=taulman, subset=TREAT=="HARVEST")
taulman.or<-oddsratios(t(harvest.tab),corr=T)
# plot the logging odds ratios
points(estimate~Comparison, data=taulman.or,type="b", pch=16)
with(taulman.or, arrows(as.numeric(Comparison), upper, as.numeric(Comparison), lower, ang=90, length=.1, code=3))
abline(h=1,lty=2)
# put in axes and titles
axis(1,at=as.numeric(taulman.or$Comparison), lab=taulman.or$Comparison)
axis(2,las=1, cex.axis=.75)
mtext("Year",1, line=3)
mtext("Odds ratio of adults", 2, line=3)
legend("topright",legend=c("Logging","Control"), pch=c(16,21), bty="n", title="Sex")
box(bty="l")

