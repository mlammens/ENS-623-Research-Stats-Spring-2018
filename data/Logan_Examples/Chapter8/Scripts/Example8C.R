###################################################
### chunk number 1: pg 188
###################################################
nelson <- read.table('nelson.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 188
###################################################
library(car)
scatterplot(WEIGHTLOSS~HUMIDITY, data=nelson)


###################################################
### chunk number 3: pg 188
###################################################
nelson.lm <- lm(WEIGHTLOSS~HUMIDITY, nelson)
plot(nelson.lm)


###################################################
### chunk number 4: pg 189
###################################################
influence.measures(nelson.lm)


###################################################
### chunk number 5: pg 190
###################################################
summary(nelson.lm)


###################################################
### chunk number 6: pg 190
###################################################
confint(nelson.lm)


###################################################
### chunk number 7: pg 190
###################################################
predict(nelson.lm, data.frame(HUMIDITY=c(50,100)), interval="prediction", se=T)


###################################################
### chunk number 8: pg 191
###################################################
#create a plot with solid dots (pch=16) and no axis or labels
plot(WEIGHTLOSS~HUMIDITY, data=nelson, pch=16, axes=F, xlab="", ylab="")
#put the x-axis (axis 1) with smaller label font size
axis(1, cex.axis=.8)
#put the x-axis label 3 lines down from the axis
mtext(text="% Relative humidity", side=1, line=3)
#put the y-axis (axis 2) with horizontal tick labels
axis(2, las=1)
#put the y-axis label 3 lines to the left of the axis
mtext(text="Weight loss (mg)", side=2, line=3)
#add the regression line from the fitted model
abline(nelson.lm)
#add the regression formula
text(99,9,"WEIGHTLOSS = -0.053HUMIDITY + 8.704", pos=2)
#add the r squared value
text(99,8.6,expression(paste(r^2==0.975)), pos=2)
#create a sequence of 1000 numbers spanning the range of humidities
x <- seq(min(nelson$HUMIDITY), max(nelson$HUMIDITY),l=1000)
#for each value of x, calculate the upper and lower 95% confidence
y<-predict(nelson.lm, data.frame(HUMIDITY=x), interval="c")
#plot the upper and lower 95% confidence limits
matlines(x,y, lty=1, col=1)
#put an L-shaped box to complete the axis
box(bty="l")
