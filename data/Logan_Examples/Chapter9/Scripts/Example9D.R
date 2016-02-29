###################################################
### chunk number 1: pg 240
###################################################
library(hier.part)
#construct a dataset entirely of predictor variables
loyn.preds <- with(loyn,data.frame(logAREA=log10(AREA), YR.ISOL, logDIST=log10(DIST), logLDIST=log10(LDIST), GRAZE, ALT))
#perform hierarchical partitioning 
hier.part(loyn$ABUND,loyn.preds, gof="Rsqu")


###################################################
### chunk number 2: pg 241
###################################################
library(hier.part)
r.HP<-rand.hp(loyn$ABUND,loyn.preds, gof="Rsqu", num.reps=100)$Iprobs
r.HP
