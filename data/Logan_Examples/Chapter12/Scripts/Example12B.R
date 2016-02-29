##################################################
### chunk number 1: pg 338
###################################################
quinn1 <- read.table('quinn1.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 338
###################################################
quinn1$DENSITY <-factor(quinn1$DENSITY)


###################################################
### chunk number 3: pg 338
###################################################
boxplot(EGGS~DENSITY * SEASON, quinn1)


###################################################
### chunk number 4: pg 339
###################################################
replications(EGGS~DENSITY*SEASON, quinn1)
library(biology)
is.balanced(EGGS~DENSITY*SEASON, quinn1)


###################################################
### chunk number 5: pg 339
###################################################
contrasts(quinn1$DENSITY) <- cbind(c(1,-.5,-.5))


###################################################
### chunk number 6: pg 339
###################################################
quinn1.aov <- aov(EGGS~DENSITY*SEASON,data=quinn1)


###################################################
### chunk number 7: pg 339
###################################################
plot(quinn1.aov, which=1)


###################################################
### chunk number 8: pg 340
###################################################
library(biology)
quinn1.anova<-AnovaM(quinn1.aov, type="I", split=list(DENSITY=list('6 vs 12&24'=1)))
quinn1.anova


###################################################
### chunk number 9: pg 340
###################################################
options(op)


###################################################
### chunk number 10: pg 340
###################################################
# effect of density in spring
library(biology)
AnovaM(mainEffects(quinn1.aov, at=SEASON=="spring"), split=list(DENSITY=list('6 vs 12&24'=1)))
# effect of density in summer
AnovaM(mainEffects(quinn1.aov, at=SEASON=="summer"), split=list(DENSITY=list('6 vs 12&24'=1)))


###################################################
### chunk number 11: pg 341
###################################################
# calculate mean and se of each combination
library(gmodels)
quinn1.means<-tapply(quinn1$EGGS,list(quinn1$DENSITY,quinn1$SEASON),mean)
quinn1.se<-tapply(quinn1$EGGS,list(quinn1$DENSITY,quinn1$SEASON), function(x) ci(x)[4])
# create a numeric version of the density variable
quinn1$DENS<-as.numeric(as.character(quinn1$DENSITY))
plot(EGGS~DENS, quinn1, type="n", axes=F, xlab="", ylab="")
# plot the points and error bars of the winter-spring season
points(quinn1.means[,1]~unique(quinn1$DENS), pch=16, type="b", lwd=2)
arrows(unique(quinn1$DENS), quinn1.means[,1]-quinn1.se[,1], unique(quinn1$DENS), 
quinn1.means[,1]+quinn1.se[,1], code=3, angle=90, len=.1)
# plot the points and error bars of the summer-autumn season
points(quinn1.means[,2]~unique(quinn1$DENS),pch=16, type="b", lwd=2, lty=2)
arrows(unique(quinn1$DENS), quinn1.means[,2]-quinn1.se[,2], unique(quinn1$DENS), 
quinn1.means[,2]+quinn1.se[,2], code=3, angle=90, len=.1)
# create the axes and their labels
axis(1,cex.axis=.8)
mtext(text="Adult Density", 1, line=3)
axis(2, cex.axis=.8,las=1)
mtext(text="Egg production", side=2, line=3)
# include a legend
legend("topright",leg=c("Winter-spring","Summer-autumn"),lwd=2,lty=c(1,2), bty="n")
box(bty="l")
