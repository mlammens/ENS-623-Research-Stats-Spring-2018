###################################################
### chunk number 1: pg 335
###################################################
quinn <- read.table('quinn.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 335
###################################################
class(quinn$DENSITY)


###################################################
### chunk number 3: pg 335
###################################################
quinn$DENSITY <-factor(quinn$DENSITY)
class(quinn$DENSITY)


###################################################
### chunk number 4: pg 335
###################################################
boxplot(EGGS~DENSITY, quinn)


###################################################
### chunk number 5: pg 335
###################################################
boxplot(EGGS~SEASON, quinn)


###################################################
### chunk number 6: pg 335
###################################################
boxplot(EGGS~DENSITY * SEASON, quinn)


###################################################
### chunk number 7: pg 335
###################################################
replications(EGGS~DENSITY*SEASON, quinn)


###################################################
### chunk number 8: pg 336
###################################################
library(biology)
is.balanced(EGGS~DENSITY*SEASON, quinn)


###################################################
### chunk number 9: pg 336
###################################################
contrasts(quinn$DENSITY) <- contr.poly(4,scores=c(8,15,30,45))


###################################################
### chunk number 10: pg 336
###################################################
quinn.aov <- aov(EGGS~DENSITY+SEASON+DENSITY:SEASON, data=quinn)
#OR equivalently,
quinn.aov <- aov(EGGS~DENSITY*SEASON,data=quinn)


###################################################
### chunk number 11: pg 336
###################################################
plot(quinn.aov, which=1)


###################################################
### chunk number 12: pg 336
###################################################
summary(quinn.aov, split=list(DENSITY=list('Linear'=1, 'Quadratic'=2)))


###################################################
### chunk number 13: pg 337
###################################################
library(biology)
AnovaM(quinn.aov, type="I", split=list(DENSITY=list('Linear'=1, 'Quadratic'=2)))


###################################################
### chunk number 14: pg 337
###################################################
# calculate mean and se of each combination
library(gmodels)
quinn.means<-tapply(quinn$EGGS,list(quinn$DENSITY,quinn$SEASON),mean)
quinn.se<-tapply(quinn$EGGS,list(quinn$DENSITY,quinn$SEASON), function(x) ci(x)[4])
# create a numeric version of the density variable
quinn$DENS<-as.numeric(as.character(quinn$DENSITY))
plot(EGGS~DENS, quinn, type="n", axes=F, xlab="", ylab="")
# plot the points and error bars of the winter-spring season
points(quinn.means[,1]~unique(quinn$DENS), pch=16, type="b", lwd=2)
arrows(unique(quinn$DENS), quinn.means[,1]-quinn.se[,1], unique(quinn$DENS), 
quinn.means[,1]+quinn.se[,1], code=3, angle=90, len=.1)
# plot the points and error bars of the summer-autumn season
points(quinn.means[,2]~unique(quinn$DENS),pch=16, type="b", lwd=2, lty=2)
arrows(unique(quinn$DENS), quinn.means[,2]-quinn.se[,2], unique(quinn$DENS), 
quinn.means[,2]+quinn.se[,2], code=3, angle=90, len=.1)
# create the axes and their labels
axis(1,cex.axis=.8)
mtext(text="Adult Density", 1, line=3)
axis(2, cex.axis=.8,las=1)
mtext(text="Egg production", side=2, line=3)
# include a legend
legend("topright",leg=c("Winter-spring","Summer-autumn"),lwd=2,lty=c(1,2), bty="n")
box(bty="l")
