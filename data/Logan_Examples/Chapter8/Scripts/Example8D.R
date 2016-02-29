###################################################
### chunk number 1: pg 192
###################################################
peake <- read.table('peake.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 192
###################################################
library(car)
scatterplot(INDIV~AREA, data=peake)


###################################################
### chunk number 3: pg 192
###################################################
library(car)
scatterplot(log10(INDIV)~log10(AREA), data=peake)


###################################################
### chunk number 4: pg 193
###################################################
peake.lm <- lm(INDIV~AREA, data=peake)
plot(peake.lm)


###################################################
### chunk number 5: pg 193
###################################################
peake.lm <- lm(log10(INDIV)~log10(AREA), data=peake)
plot(peake.lm)


###################################################
### chunk number 6: pg 193
###################################################
influence.measures(peake.lm)


###################################################
### chunk number 7: pg 194
###################################################
summary(peake.lm)


###################################################
### chunk number 8: pg 195
###################################################
#create a plot with solid dots (pch=16) and no axis or labels}
plot(INDIV~AREA, data=peake, pch=16, axes=F, xlab="", ylab="",log="xy")
#put the x-axis (axis 1) with smaller label font size
axis(1, cex.axis=.8)
#put the x-axis label 3 lines down from the axis
mtext(text=expression(paste("Mussel clump area", (mm^2))), side=1, line=3)
#put the y-axis (axis 2) with horizontal tick labels
axis(2, las=1)
#put the y-axis label 3 lines to the left of the axis
mtext(text="Number of individuals", side=2, line=3)
#add the regression line from the fitted model
abline(peake.lm)
#add the regression formula
text(30000, 30, expression(paste(log[10], "INDIV = 0.835", log[10], "AREA - 0.576")), pos=2)
#add the r squared value
text(30000, 22, expression(paste(r^2==0.835)), pos=2)
#put an L-shaped box to complete the axis
box(bty="l")


###################################################
### chunk number 9: pg 195
###################################################
10^predict(peake.lm, data.frame(AREA=c(8000,10000)))


###################################################
### chunk number 10: pg 196
###################################################
10^predict(peake.lm,data.frame(AREA=c(8000,10000)), interval="prediction")


###################################################
### chunk number 11: pg 196
###################################################
plot(log10(INDIV)~log10(AREA), data=peake, pch=16, axes=F, xlab="", ylab="")
axis(1, cex.axis=.8)
mtext(text="Log Mussel clump area", side=1, line=3)
axis(2, las=1)
mtext(text="Log number of individuals", side=2, line=3)
abline(peake.lm)
text(4.5,1.4,expression(paste(log[10],"INDIV = 0.835",log[10],"AREA - 0.576")), pos=2)
text(4.5,1.3,expression(paste(r^2==0.835)), pos=2)
x <- seq(min(peake$AREA), max(peake$AREA),l=1000)
y<-predict(peake.lm,data.frame(AREA=x), interval="c")
matlines(log10(x),y, lty=1, col=1)
box(bty="l")

