###################################################
### chunk number 1: pg 186
###################################################
green <- read.table('green.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 186
###################################################
library(car)
scatterplot(BURROWS~TOTMASS, data=green, subset=SITE=='LS', reg.line=F)


###################################################
### chunk number 3: pg 186
###################################################
library(car)
scatterplot(BURROWS~TOTMASS, data=green, subset=SITE=='DS', reg.line=F)


###################################################
### chunk number 4: pg 186
###################################################
cor.test(~BURROWS + TOTMASS, data=green, subset=SITE=='LS', method='spearman')


###################################################
### chunk number 5: pg 187
###################################################
cor.test(~BURROWS + TOTMASS, data=green, subset=SITE=='DS', method='spearman')


###################################################
### chunk number 6: pg 187
###################################################
plot(BURROWS~TOTMASS, data=green, subset=SITE=='LS', xlim=c(0,8), ylim=c(0,80))
with(subset(green, SITE=='LS'), data.ellipse(TOTMASS, BURROWS, levels=.95, add=T))


###################################################
### chunk number 7: pg 187
###################################################
plot(BURROWS~TOTMASS, data=green, subset=SITE=='DS', xlim=c(0,8), ylim=c(0,150))
with(subset(green, SITE=='DS'), data.ellipse(TOTMASS, BURROWS, levels=.95, add=T))
