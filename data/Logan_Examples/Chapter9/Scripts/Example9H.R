###################################################
### chunk number 1: pg 251
###################################################
library(tree)
loyn.tree <- tree(ABUND~AREA+YR.ISOL+DIST+LDIST+GRAZE+ALT, data=loyn, mindev=0)


###################################################
### chunk number 2: pg 252
###################################################
plot(residuals(loyn.tree)~predict(loyn.tree))


###################################################
### chunk number 3: pg 252
###################################################
plot(loyn.tree, type="uniform")
text(loyn.tree,cex=.5, all=T)
text(loyn.tree,lab=paste("n"),cex=.5,adj=c(0,2), splits=F)


###################################################
### chunk number 4: pg 253
###################################################
plot(prune.tree(loyn.tree))

###################################################
### chunk number 5: pg 253
###################################################
getOption("SweaveHooks")[["fig"]]()
loyn.tree.prune<-prune.tree(loyn.tree, best=3)
plot(loyn.tree.prune, type="uniform")
text(loyn.tree.prune,cex=.5, all=T)
text(loyn.tree.prune,lab=paste("n"),cex=.5,adj=c(0,2), splits=F)
