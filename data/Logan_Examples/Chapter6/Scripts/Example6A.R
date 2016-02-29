###################################################
### chunk number 1: pg 142
###################################################
ward <- read.table('ward.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 142
###################################################
boxplot(EGGS~ZONE, ward)


###################################################
### chunk number 3: pg 143
###################################################
with(ward, rbind(MEAN=tapply(EGGS, ZONE, mean), VAR=tapply(EGGS,ZONE,var)))


###################################################
### chunk number 4: pg 143
###################################################
t.test(EGGS~ZONE, ward, var.equal=T)


###################################################
### chunk number 5: pg 143
###################################################
# calculate mean and se for each zone
ward.means<-with(ward, tapply(EGGS, ZONE, mean))
ward.sds<-with(ward, tapply(EGGS, ZONE, sd))
ward.ns<-with(ward, tapply(EGGS, ZONE, length))
ward.se <- ward.sds/sqrt(ward.ns)
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

