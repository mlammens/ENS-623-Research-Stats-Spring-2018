###################################################
### chunk number 1: pg 356
###################################################
milliken <- read.table('milliken.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 356
###################################################
boxplot(VOL~FAT*SURF, milliken)


###################################################
### chunk number 3: pg 356
###################################################
replications(VOL~FAT*SURF, milliken)
library(biology)
is.balanced(VOL~FAT*SURF, milliken)


###################################################
### chunk number 4: pg 357
###################################################
milliken$FS<-as.factor(paste(milliken$FAT, milliken$SURF, sep=""))


###################################################
### chunk number 5: pg 357
###################################################
contrasts(milliken$FS)<-cbind(c(1,1,0,0,-1,-1,0), c(0,0,1,1,-1,0,-1))
AnovaM(aov(VOL~FS, milliken),split=list(FS=list("fat"=1:2, " fat: 1 vs 3" =1," fat 2 vs 3"=2)), type="III")


###################################################
### chunk number 6: pg 358
###################################################
contrasts(milliken$FS)<-cbind(c(0,0,0,0,0,1,-1), c(0,0,1,-1,1,0,-1))
AnovaM(aov(VOL~FS, milliken),split=list(FS=list("surf"=1:2, " surf: 2 vs 3" = 1, " surf: 1 vs 3"=2)), type="III")


###################################################
### chunk number 7: pg 358
###################################################
contrasts(milliken$FS)<-cbind(c(1,-1,0,0,-1,1,0), c(0,0,1,-1,-1,0,1))
AnovaM(aov(VOL~FS, milliken),split=list(FS=list("fat:surf"=1:2, " fat:surf1"=1, " fat:surf2"=2)), type="III")

###################################################
### chunk number 8: pg 358
###################################################
# calculate mean and se of each combination
library(gmodels)
milliken.means<-with(milliken,tapply(VOL,list(SURF,FAT),mean, na.rm=T))
milliken.se<-with(milliken,tapply(VOL,list(SURF,FAT), function(x) ci(x, na.rm=T)[4]))
xs <- barplot(milliken.means,ylim=range(milliken$VOL, na.rm=T),beside=T,axes=F, xpd=F,axisnames=F,axis.lty=2,legend.text=F, col=c(0,1,"gray"))
axis(2,las=1)
axis(1,at=apply(xs,2,median), lab=c("Fat 1", "Fat 2", "Fat 3"), padj=1, mgp=c(0,0,0))
mtext(2,text=expression(paste("Mean specific bread volume ")),line=3, cex=1)
#mtext(1,text="Surfactant type",line=3, cex=1)
box(bty="l")
arrows(xs,milliken.means,xs,milliken.means+milliken.se, code=2, angle=90, len=.05)
legend("topleft",leg=c("Surfactant 1", "Surfactant 2", "Surfactant 3"), fill=c(0,1,"gray"), col=c(0,1,"gray"), bty="n", cex=1)
