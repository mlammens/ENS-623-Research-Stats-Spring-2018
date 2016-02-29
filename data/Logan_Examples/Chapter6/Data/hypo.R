###################################################
### chunk number 1: pg 142
###################################################
ward <- read.table('ward.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 142
###################################################
boxplot(EGGS~ZONE, ward)


###################################################
### chunk number 21: WardMeans
###################################################
with(ward, rbind(MEAN=tapply(EGGS, ZONE, mean), VAR=tapply(EGGS,ZONE,var)))


###################################################
### chunk number 22: WardTtest
###################################################
t.test(EGGS~ZONE, ward, var.equal=T)


###################################################
### chunk number 23: WardBarPlot
###################################################
# calculate mean and se for each zone
ward.means<-with(ward, tapply(EGGS, ZONE, mean))
ward.sds<-with(ward, tapply(EGGS, ZONE, sd))
ward.ns<-with(ward, tapply(EGGS, ZONE, length))
ward.se <- ward.sds/sqrt(ward.ns)
#ward.se <-with(ward, tapply(EGGS, ZONE, function(x) ci(x)[4]))
xs <- barplot(ward.means, ylim=range(pretty(c(ward.means+ward.se, 
ward.means-ward.se))), axes=F, xpd=F, axisnames=F, axis.lty=2, legend.text=F, 
col="gray")
# plot the error bars of the winter-spring season
arrows(xs,ward.means+ward.se,xs,ward.means-ward.se, code=3, angle=90, len=.05)
# create the axes and their labels
axis(2,las=1)
axis(1,at=xs, lab=c("Littorinid","Mussel"), padj=1, mgp=c(0,0,0))
mtext(2,text="Mean number of egg capsules per capsule",line=3, cex=1)
mtext(1,text="Zone",line=3, cex=1)
box(bty="l")


###################################################
### chunk number 24: FurnessReadTable
###################################################
furness <- read.table('furness.csv', header=T, sep=',')


###################################################
### chunk number 25: FurnessBoxplot
###################################################
boxplot(METRATE~SEX, furness)


###################################################
### chunk number 26: FurnessMeans
###################################################
with(furness, rbind(MEAN=tapply(METRATE, SEX, mean), VAR=tapply(METRATE, SEX, var)))


###################################################
### chunk number 27: FurnessTtest
###################################################
t.test(METRATE~SEX, furness, var.equal=F)


###################################################
### chunk number 28: ElgarReadTable
###################################################
elgar <- read.table('elgar.csv', header=T, sep=',')


###################################################
### chunk number 29: ElgarBoxplot1
###################################################
with(elgar, boxplot(HORIZLIG -
HORIZDIM))


###################################################
### chunk number 30: ElgarBoxplot2
###################################################
with(elgar, boxplot(VERTLIGH -
VERTDIM))


###################################################
### chunk number 31: ElgarTtest1
###################################################
with(elgar, t.test(HORIZLIG,HORIZDIM, paired=T))


###################################################
### chunk number 32: ElgarTtest2
###################################################
with(elgar, t.test(VERTLIGH,VERTDIM, paired=T))


###################################################
### chunk number 33: FurnessReadTable
###################################################
nymphs <- read.table('nymphs.csv', header=T, sep=',')


###################################################
### chunk number 34: nymphsBoxplot
###################################################
boxplot(LENGTH~SAMPLE, nymphs)


###################################################
### chunk number 35: FurnessMeans
###################################################
with(nymphs, rbind(MEAN=tapply(LENGTH, SAMPLE, mean), 
VAR=tapply(LENGTH, SAMPLE, var)))


###################################################
### chunk number 36: FurnessTtest
###################################################
wilcox.test(LENGTH~SAMPLE, nymphs)


###################################################
### chunk number 37: BeetleReadTable
###################################################
beetles <- read.table('beetle.csv', header=T, sep=',')


###################################################
### chunk number 38: BeetleBoxplot
###################################################
boxplot(BEETLES~SIZE, 
beetles)


###################################################
### chunk number 39: BeetleBoxplot1
###################################################
boxplot(sqrt(BEETLES)~SIZE, 
beetles)


###################################################
### chunk number 40: BeetleStat
###################################################
stat <- function(data, indices) {
t.test <- t.test(BEETLES~SIZE, data)$"stat"
t.test
}


###################################################
### chunk number 41: BeetleRand
###################################################
rand.gen <- function(data,mle) {
out <- data
out$SIZE <- sample(out$SIZE, replace=F)
out
}


###################################################
### chunk number 42: JUnk
###################################################
library(boot)


###################################################
### chunk number 43: BeetleBoot
###################################################
beetles.boot <- boot(beetles, stat, R=5000, sim="parametric", ran.gen=rand.gen)


###################################################
### chunk number 44: BeetleBoot1a
###################################################
print(beetles.boot)


###################################################
### chunk number 45: BeetlesBootPlot
###################################################
plot(beetles.boot)


###################################################
### chunk number 46: BeetleTval
###################################################
tval <- length(beetles.boot[beetles.boot$t >= abs(beetles.boot$t0)])+1
tval/(beetles.boot$R + 1)


###################################################
### chunk number 47: CreateRFile
###################################################
Stangle("../../hypo.rnw")


