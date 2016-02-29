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


###################################################
### chunk number 48: ParueloReadTable
###################################################
paruelo <- read.table('paruelo.csv', header=T, sep=',')


###################################################
### chunk number 49: parueloScatterplotMatrix1
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
scatterplot.matrix(~C3+MAP+MAT+JJAMAP+DJFMAP+LONG+LAT, data=paruelo,
diag="boxplot")


###################################################
### chunk number 50: parueloScatterplotMatrix2
###################################################
getOption("SweaveHooks")[["fig"]]()
scatterplot.matrix(~log10(C3+0.1)+MAP+MAT+JJAMAP+DJFMAP+LONG+LAT,
data=paruelo, diag="boxplot")


###################################################
### chunk number 51: ParueloCor
###################################################
cor(paruelo[,2:7])


###################################################
### chunk number 52: ParueloVIF
###################################################
vif(lm(log10(C3+.1)~MAP+MAT+JJAMAP+DJFMAP+LONG+LAT, data=paruelo))
1/vif(lm(log10(C3+.1)~MAP+MAT+JJAMAP+DJFMAP+LONG+LAT, data=paruelo))


###################################################
### chunk number 53: ParueloVIF1
###################################################
1/vif(lm(log10(C3+.1)~LAT+LONG+LAT:LONG, data=paruelo))


###################################################
### chunk number 54: ParueloCenter
###################################################
paruelo$cLAT <- paruelo$LAT-mean(paruelo$LAT)
#OR
paruelo$cLAT <- scale(paruelo$LAT, scale=F)
paruelo$cLONG <- scale(paruelo$LONG, scale=F)
1/vif(lm(log10(C3+.1)~cLAT+cLONG+cLAT:cLONG, data=paruelo))


###################################################
### chunk number 55: ParueloFigHooks
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(2,2))))


###################################################
### chunk number 56: parueloResid
###################################################
getOption("SweaveHooks")[["fig"]]()
paruelo.lm<-lm(log10(C3+.1)~cLAT+cLONG+cLAT:cLONG, data=paruelo)
plot(paruelo.lm)


###################################################
### chunk number 57: ParueloFigHooks1
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(1,1))))


###################################################
### chunk number 58: ParueloInfluenceMeasures eval=FALSE
###################################################
## influence.measures(paruelo.lm)


###################################################
### chunk number 59: ParueloInfluenceMeasures1
###################################################
head(influence.measures(paruelo.lm)[[1]])
cat('...\n')


###################################################
### chunk number 60: ParueloSummary
###################################################
summary(paruelo.lm)


###################################################
### chunk number 61: ParueloSD1
###################################################
LAT_sd1<-mean(paruelo$cLAT)-2*sd(paruelo$cLAT)
paruelo_LONG.lm1<-lm(log10(C3+.1)~cLONG*c(cLAT-LAT_sd1), data=paruelo)
summary(paruelo_LONG.lm1)


###################################################
### chunk number 62: ParueloSD2
###################################################
LAT_sd2<-mean(paruelo$cLAT)-1*sd(paruelo$cLAT)
paruelo_LONG.lm2<-lm(log10(C3+.1)~cLONG*c(cLAT-LAT_sd2), data=paruelo)
summary(paruelo_LONG.lm2)


###################################################
### chunk number 63: ParueloSD4
###################################################
LAT_sd4<-mean(paruelo$cLAT)-1*sd(paruelo$cLAT)
paruelo_LONG.lm4<-lm(log10(C3+.1)~cLONG*c(cLAT-LAT_sd4), data=paruelo)
summary(paruelo_LONG.lm4)


###################################################
### chunk number 64: ParueloSD5
###################################################
LAT_sd5<-mean(paruelo$cLAT)-1*sd(paruelo$cLAT)
paruelo_LONG.lm5<-lm(log10(C3+.1)~cLONG*c(cLAT-LAT_sd5), data=paruelo)
summary(paruelo_LONG.lm5)


###################################################
### chunk number 65: SubJunk
###################################################
op<-options(width=120)


###################################################
### chunk number 66: Subset3 eval=FALSE
###################################################
## library(biology)
## m<-Model.selection(loyn.lm)
## Model.selection(loyn.lm)[[1]][1:45,c(2,5,6,7)]


###################################################
### chunk number 67: Subset4
###################################################
#Model.selection(loyn.lm)[[1]][1:45,c(2,5,6,7)]
txt <- capture.output( {
library(biology)
m<-Model.selection(loyn.lm)
Model.selection(loyn.lm)[[1]][1:45,c(2,5,6,7)]
} )
if( length(txt) > 10 ){
    #txt <- c( txt[1:10], "..." , txt[25:30], "...", txt[40:45], "...", txt[68], " ", txt[135:145])
    txt <- c( txt[1:10], "..." , txt[25:30], "...", txt[40:45], "...")
}
#cat( txt, sep = "\n" )
#m[[2]]


###################################################
### chunk number 68: Subjunk1
###################################################
options(op)


###################################################
### chunk number 69: ModelSelectionA eval=FALSE
###################################################
## library(biology)
## Model.selection(loyn.lm)


###################################################
### chunk number 70: ModelSelectionB
###################################################
cat( txt, sep = "\n" )
m[[2]]


###################################################
### chunk number 71: SubJunk2
###################################################
op<-options(width=120)


###################################################
### chunk number 72: Subset32
###################################################
library(MuMIn)
model.avg(get.models(dredge(loyn.lm, rank="AIC")))


###################################################
### chunk number 73: Subjunk12
###################################################
options(op)


###################################################
### chunk number 74: LoynLM2
###################################################
loyn.lm2<-lm(ABUND~log10(AREA)+GRAZE, data=loyn)
summary(loyn.lm2)


###################################################
### chunk number 75: LoynHPcache
###################################################
library(hier.part)
#loyn.preds <- loyn.lm$model[,-1]
loyn.preds <- with(loyn,data.frame(logAREA=log10(AREA), YR.ISOL,
logDIST=log10(DIST), logLDIST=log10(LDIST), GRAZE, ALT))
hp<-hier.part(loyn$ABUND,loyn.preds, gof="Rsqu")


###################################################
### chunk number 76: LoynHP1 eval=FALSE
###################################################
## library(hier.part)
## #construct a dataset entirely of predictor variables
## loyn.preds <- with(loyn,data.frame(logAREA=log10(AREA), YR.ISOL,
## logDIST=log10(DIST), logLDIST=log10(LDIST), GRAZE, ALT))
## #perform hierarchical partitioning 
## hier.part(loyn$ABUND,loyn.preds, gof="Rsqu")


###################################################
### chunk number 77: LoynHP2
###################################################
hp


###################################################
### chunk number 78: LoynRandHPcache
###################################################
library(hier.part)
r.HP<-rand.hp(loyn$ABUND,loyn.preds, gof="Rsqu", num.reps=100)$Iprobs


###################################################
### chunk number 79: LoynRandHP1 eval=FALSE
###################################################
## r.HP<-rand.hp(loyn$ABUND,loyn.preds, gof="Rsqu", num.reps=100)$Iprobs


###################################################
### chunk number 80: LoynRandHP2
###################################################
r.HP


###################################################
### chunk number 81: McKechnieAReadTable
###################################################
mckechnie2 <- read.table('mckechnie2.csv', header=T, sep=',')


###################################################
### chunk number 82: McKechnie2VIF
###################################################
library(car)
vif(lm(HK~PRECIP+MAXTEMP+MINTEMP+ALT, mckechnie2))


###################################################
### chunk number 83: MeKechnie2LM
###################################################
mckechnie2.lm<-lm(HK~PRECIP+MAXTEMP+MINTEMP+ALT, mckechnie2)
summary(mckechnie2.lm)


###################################################
### chunk number 84: McKechnie2Stat
###################################################
stat <- function(data, indices) {
summary(lm(HK~PRECIP+MAXTEMP+MINTEMP+ALT, data))$coef[,3]
}


###################################################
### chunk number 85: McKechnie2RandGen
###################################################
rand.gen<- function(data,mle) {
out<-data
out$HK <-sample(out$HK, replace=F)
out
}


###################################################
### chunk number 86: McKechnie2BootLib
###################################################
library(boot)


###################################################
### chunk number 87: McKechnie2Bootcache
###################################################
#library(boot)
mckechnie2.boot<-boot(mckechnie2, stat, R=1000, sim="parametric",
ran.gen=rand.gen)


###################################################
### chunk number 88: McKechnie2Boot1 eval=FALSE
###################################################
## library(boot)
## mckechnie2.boot<-boot(mckechnie2, stat, R=1000, sim="parametric",
## ran.gen=rand.gen)


###################################################
### chunk number 89: McKechnie2T
###################################################
t<-apply(apply(abs(mckechnie2.boot$t), 1, ">=", abs(mckechnie2.boot$t0))
*1, 1, 
"sum")+1
t/(mckechnie2.boot$R+1)


###################################################
### chunk number 90: McKechnie2Stat2cache
###################################################
stat <- function(data, indices) {
summary(lm(HK~PRECIP+MAXTEMP+MINTEMP+ALT, data))$fstatistic
}
rand.gen<- function(data,mle) {
out<-data
out$HK <-sample(out$HK, replace=F)
out
}
mckechnie2.boot<-boot(mckechnie2, stat, R=1000, sim="parametric", 
ran.gen=rand.gen)
f<-apply(apply(abs(mckechnie2.boot$t), 1, ">=", abs(mckechnie2.boot$t0))
*1, 1, 
"sum")+1
FF<-f/(mckechnie2.boot$R+1)


###################################################
### chunk number 91: McKechnie2Stat2a eval=FALSE
###################################################
## stat <- function(data, indices) {
## summary(lm(HK~PRECIP+MAXTEMP+MINTEMP+ALT, data))$fstatistic
## }
## rand.gen<- function(data,mle) {
## out<-data
## out$HK <-sample(out$HK, replace=F)
## out
## }
## mckechnie2.boot<-boot(mckechnie2, stat, R=1000, sim="parametric", 
## ran.gen=rand.gen)
## f<-apply(apply(abs(mckechnie2.boot$t), 1, ">=", abs(mckechnie2.boot$t0))
## *1, 1, 
## "sum")+1
## f/(mckechnie2.boot$R+1)


###################################################
### chunk number 92: McKechnie2Stat2b
###################################################
FF


###################################################
### chunk number 93: MytilusReadTable
###################################################
mytilus <- read.table('mytilus.csv', header=T, sep=',')


###################################################
### chunk number 94: TEMP
###################################################
op <- options(width=35)


###################################################
### chunk number 95: mytilusScatterplot2
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
scatterplot(asin(sqrt(LAP))*180/pi ~DIST, 
data=mytilus)


###################################################
### chunk number 96: mytilusResidualPlot
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(lm(asin(sqrt(LAP))*180/pi ~DIST, 
data=mytilus), which=1)


###################################################
### chunk number 97: TEMP1
###################################################
options(op)


###################################################
### chunk number 98: MytilusLM
###################################################
mytilus.lm5<- lm(asin(sqrt(LAP))*180/pi ~ DIST + I(DIST^2) +
I(DIST^3) + I(DIST^4) + I(DIST^5), mytilus)


###################################################
### chunk number 99: mytilusResidualPlot2
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(mytilus.lm5, which=1)


###################################################
### chunk number 100: MytilusAnova
###################################################
anova(mytilus.lm5)


###################################################
### chunk number 101: MytilusLM1
###################################################
mytilus.lm1<- lm(asin(sqrt(LAP))*180/pi ~DIST, mytilus)
mytilus.lm2<- lm(asin(sqrt(LAP))*180/pi ~DIST+I(DIST^2), mytilus)
anova(mytilus.lm2, mytilus.lm1)


###################################################
### chunk number 102: MytilusLM2
###################################################
mytilus.lm3<- lm(asin(sqrt(LAP))*180/pi ~DIST+I(DIST^2)+I(DIST^3), mytilus)
anova(mytilus.lm3, mytilus.lm2)


###################################################
### chunk number 103: MytilusSummary
###################################################
summary(mytilus.lm3)


###################################################
### chunk number 104: mytilusPlot
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(asin(sqrt(LAP))*180/pi ~ DIST, data=mytilus,pch=16, axes=F, xlab="", ylab="")
axis(1, cex.axis=.8)
mtext(text=expression(paste("Miles east of Southport, Connecticut")),
side=1, line=3)
axis(2, las=1)
mtext(text=expression(paste("Arcsin ",sqrt(paste("freq. of allele ", 
italic("Lap"))^{94}))), side=2, line=3)
x<-seq(0,80,l=1000)
points(x,predict(mytilus.lm3, data.frame(DIST=x)), type="l")
box(bty="l")


###################################################
### chunk number 105: Peake1ReadTable
###################################################
peake <- read.table('peake.csv', header=T, sep=',')


###################################################
### chunk number 106: TEMP
###################################################
op <- options(width=35)


###################################################
### chunk number 107: peake1Scatterplot
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
scatterplot(SPECIES~AREA, data=peake)


###################################################
### chunk number 108: peake1ResidualPlot
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(lm(SPECIES~AREA, data=peake), which=1)


###################################################
### chunk number 109: TEMP1
###################################################
options(op)


###################################################
### chunk number 110: PeakeNLS
###################################################
peake.nls <- nls(SPECIES~alpha*AREA^beta, start=list(alpha=0.1, beta=1),
peake)


###################################################
### chunk number 111: peake1ResidualPlot2
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(resid(peake.nls)~fitted(peake.nls))


###################################################
### chunk number 112: PeakeNLSSummary
###################################################
summary(peake.nls)


###################################################
### chunk number 113: PeakeAIC
###################################################
AIC(peake.nls, k=log(nrow(peake))) #BIC
AIC(peake.nls) #AIC
deviance(peake.nls)/df.residual(peake.nls) #MSresid
peake.lm<-lm(SPECIES~AREA, data=peake) #linear fit
AIC(peake.lm, k=log(nrow(peake))) #lm BIC
AIC(peake.lm) #lm AIC
deviance(peake.lm)/df.residual(peake.lm) #lm MSresid


###################################################
### chunk number 114: PeakeNLSAsym
###################################################
peake.nls1 <- nls(SPECIES~SSasymp(AREA,a,b,c),peake)
summary(peake.nls1)
AIC(peake.nls1) #AIC
deviance(peake.nls1)/df.residual(peake.nls1) #MSresid
anova(peake.nls,peake.nls1)


###################################################
### chunk number 115: peakePlot
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(SPECIES~AREA, peake, pch=16, axes=F, xlab="", ylab="")
axis(1, cex.axis=.8)
mtext(text=expression(paste("Clump area ", (dm^2))), side=1, line=3)
axis(2, las=1)
mtext(text="Number of species", side=2, line=3)
box(bty="l")
x <- seq(0,30000, l=1000)
points(x, predict(peake.nls, data.frame(AREA=x)), type='l', lty=2)
points(x, predict(nls(SPECIES~SSasymp(AREA,a,b,c),peake), data.frame(AREA=x)), 
type='l', lty=1)
box(bty="l")


###################################################
### chunk number 116: LoynTree
###################################################
library(tree)
loyn.tree <- tree(ABUND~AREA+YR.ISOL+DIST+LDIST+GRAZE+ALT, data=loyn, mindev=0)


###################################################
### chunk number 117: loynResid3
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(residuals(loyn.tree)~predict(loyn.tree))


###################################################
### chunk number 118: loynTree
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(loyn.tree, type="uniform")
text(loyn.tree,cex=.5, all=T)
text(loyn.tree,lab=paste("n"),cex=.5,adj=c(0,2), splits=F)


###################################################
### chunk number 119: loynTreePrune
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(prune.tree(loyn.tree))


###################################################
### chunk number 120: loynPrunedTree
###################################################
getOption("SweaveHooks")[["fig"]]()
loyn.tree.prune<-prune.tree(loyn.tree, best=3)
plot(loyn.tree.prune, type="uniform")
text(loyn.tree.prune,cex=.5, all=T)
text(loyn.tree.prune,lab=paste("n"),cex=.5,adj=c(0,2), splits=F)


###################################################
### chunk number 121: CreateRFile
###################################################
Stangle("../../multipleRegression.rnw")


