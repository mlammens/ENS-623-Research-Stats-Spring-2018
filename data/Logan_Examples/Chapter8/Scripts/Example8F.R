###################################################
### chunk number 1: pg 199
###################################################
christ <- read.table('christ.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 200
###################################################
scatterplot(CWD.BASA~RIP.DENS, data=christ)


###################################################
### chunk number 3: pg 200
###################################################
library(biology)
christ.lm <- lm.II(CWD.BASA~RIP.DENS, christ, type="RMA")
summary(christ.lm)


###################################################
### chunk number 4: pg 200
###################################################
#create a plot with solid dots (pch=16) and no axis or labels
plot(CWD.BASA~RIP.DENS, christ, pch=16, axes=F, xlab="", ylab="")
#put the x-axis (axis 1) with smaller label font size
axis(1, cex.axis=.8)
#put the x-axis label 3 lines down from the axis
mtext(text="Riparian tree density", side=1, line=3)
#put the y-axis (axis 2) with horizontal tick labels
axis(2, las=1)
#put the y-axis label 3 lines to the left of the axis
mtext(text="Course woody debris basal area", side=2, line=3)
#add the regression line from the fitted model
abline(christ.lm)
#add the regression parameters
text(1600,50,expression(paste(beta[1]==0.145)), pos=4)
text(1600,40,expression(paste(beta[0]==-113.904)), pos=4)
#put an L-shaped box to complete the axis
box(bty="l")


