###################################################
### chunk number 1: pg 274
###################################################
purves <- read.table('purves.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 275
###################################################
boxplot(LENGTH~TREAT, data=purves)


###################################################
### chunk number 3: pg 275
###################################################
kruskal.test(LENGTH~TREAT, data=purves)


###################################################
### chunk number 4: pg 275
###################################################
library(npmc)
dat <- data.frame(var=purves$LENGTH, class=purves$TREAT)
ss<-summary(npmc(dat),type='Steel')


###################################################
### chunk number 5: pg 276
###################################################
means <- with(purves, tapply(LENGTH, TREAT, mean, na.rm =T))
sds <- with(purves, tapply(LENGTH, TREAT, sd, na.rm =T))
n <- with(purves, tapply(LENGTH, TREAT, length))
ses <- sds/sqrt(n)
ys <- pretty(c(means - ses, means + (2 * ses)))
xs<-barplot(means, beside=T, axes=F, ann=F, ylim = c(min(ys), max(ys)), xpd=F)
arrows(xs, means+ses, xs, means-ses, ang=90, length=0.05, code=3)
axis(2, las = 1)
mtext(2, text = "Mean pea length", line = 3, cex = 1.5)
mtext(1, text = "Sugar treatment", line = 3, cex = 1.5)
text(xs, means + ses, labels = c('A','B','B','B','C'), pos = 3)
box(bty="l")
