###################################################
### chunk number 1: pg 224
###################################################
loyn <- read.table('loyn.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 225
###################################################
library(car)
scatterplot.matrix(~ABUND+AREA+YR.ISOL+DIST+LDIST+GRAZE+ALT, data=loyn, diag="boxplot")


###################################################
### chunk number 3: pg 225
###################################################
scatterplot.matrix(~ABUND+log10(AREA)+YR.ISOL+log10(DIST)+
log10(LDIST)+GRAZE+ALT, data=loyn, diag="boxplot")


###################################################
### chunk number 4: pg 226
###################################################
cor(loyn[,2:7])


###################################################
### chunk number 5: pg 227
###################################################
vif(lm(ABUND~log10(AREA)+YR.ISOL+log10(DIST)+log10(LDIST) +GRAZE+ALT, data=loyn))
1/vif(lm(ABUND~log10(AREA)+YR.ISOL+log10(DIST)+log10(LDIST) +GRAZE+ALT, data=loyn))


###################################################
### chunk number 6: pg 227
###################################################
loyn.lm<-lm(ABUND~log10(AREA)+YR.ISOL+log10(DIST)+log10(LDIST)+GRAZE+ALT,
data=loyn)


###################################################
### chunk number 6: pg 227
###################################################
plot(loyn.lm)


###################################################
### chunk number 7: pg 228
###################################################
influence.measures(loyn.lm)


###################################################
### chunk number 8: pg 228
###################################################
summary(loyn.lm)


###################################################
### chunk number 9: pg 229
###################################################
av.plots(loyn.lm, ask=F)
