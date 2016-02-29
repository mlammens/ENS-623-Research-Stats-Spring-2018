###################################################
### chunk number 1: pg 352
###################################################
hall1 <- read.table('hall1.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 352
###################################################
hall1$TIME<- as.factor(hall1$TIME)


###################################################
### chunk number 3: pg 353
###################################################
boxplot(IND~TREAT * TIME, hall1)



###################################################
### chunk number 4: pg 353
###################################################
boxplot(log(IND+1)~TREAT * TIME, hall1)


###################################################
### chunk number 5: pg 353
###################################################
replications(log(IND+1)~TREAT*TIME, hall1)
library(biology)
is.balanced(log(IND+1)~TREAT*TIME, hall1)


###################################################
### chunk number 6: pg 354
###################################################
hall1$TREATTIME <- as.factor(paste(hall1$TREAT,hall1$TIME,sep=""))


###################################################
### chunk number 7: pg 354
###################################################
contrasts(hall1$TREATTIME) <- cbind(c(1, 1, -1, -1, 0))
AnovaM(aov(log(IND + 1) ~ TREATTIME, hall1), split = list(TREATTIME = list("treatment" = 1)))


###################################################
### chunk number 8: pg 354
###################################################
contrasts(hall1$TREATTIME) <- cbind(c(1, -1, 1, -1, 0), c(0, 0, 1, 0, -1))
AnovaM(aov(log(IND + 1) ~ TREATTIME, hall1), split = list(TREATTIME = list("time" = 1:2, " time 2 vs 4" = 1, " time 2 vs 6" = 2)))


###################################################
### chunk number 9: pg 354
###################################################
contrasts(hall1$TREATTIME) <- cbind(c(1, -1, -1, 1, 0))
AnovaM(aov(log(IND + 1) ~ TREATTIME, hall1), split = list(TREATTIME = list("treatment:time" = 1)))


###################################################
### chunk number 10: pg 355
###################################################
# calculate mean and se of each combination
library(gmodels)
hall1.means<-with(hall1, tapply(IND, list(TIME, TREAT), mean))
hall1.se<-with(hall1, tapply(IND, list(TIME,TREAT), function(x) ci(x)[4]))
with(hall1,interaction.plot(TIME, TREAT, IND, las=1, lwd=2, ylim=range(pretty(hall1$IND)), axes=F, xlab="", ylab="", pch=c(16,17), type="b", legend=F))
# plot the error bars
arrows(1:3, hall1.means-hall1.se, 1:3, hall1.means+hall1.se, code=3, angle=90, len=.05)
# create the axes and their labels
axis(2, cex.axis=.8,las=1, mgp=c(3,.5,0), tcl=-.2)
mtext(text=expression(paste("Mean number of macroinvertebrate")), side=2, line=3, cex=1)
mtext(text=expression(paste("individuals")), side=2, line=2, cex=1)
axis(1,cex.axis=.8, at=1:3,lab=c("2", "4", "6"))
mtext(text="Time (duration)", 1, line=3, cex=1)
box(bty="l")
# include a legend
legend("topright", leg=c("Control","Nutrient added"), lwd=2, lty=c(2,1), bty="n", pch=c(16,17), cex=1)
