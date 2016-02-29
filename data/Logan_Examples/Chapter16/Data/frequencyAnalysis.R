###################################################
### chunk number 1: FrequencyAnalysis_SetDir
###################################################
setwd("FrequencyAnalysis/Data")
setCacheDir("../cache")
options(SweaveHooks=list(fig=function() par(mar=c(4,4,0.1,0.1))) )


###################################################
### chunk number 2: PoissonDistribution
###################################################
getOption("SweaveHooks")[["fig"]]()
opar<-par(mar=c(4,.1,.1,.1))
x<-0:50
y<-dpois(x,25)
y1<-dpois(x,15)
y2<-dpois(x,5)
plot(x,y,type='h',ylim=c(0,max(y,y1,y2)), xlim=c(0,40), axes=F, xlab="", ylab="", lwd=5, col="gray80")
lines(x,y1, type='h',lwd=5, col="gray50")
lines(x,y2, type='h',lwd=5, col="gray0")
axis(1, at=seq(0,50,b=5))
par(opar)


###################################################
### chunk number 3: ChisquaredDistribution
###################################################
getOption("SweaveHooks")[["fig"]]()
opar<-par(mar=c(3,1,0,0))
    x<-seq(0,50,l=1000)
    plot(x,dchisq(x,1),type='l', xlim=c(0,40), ylim=c(0,.3),axes=F, xlab="", ylab="", lwd=3, col="black")
    text(5,.3,"df=1", cex=1.5)
    points(x,dchisq(x,4),type='l',lwd=3, col="black")
    text(5,.20,"df=4", cex=1.5)
    points(x,dchisq(x,10),type='l',lwd=3, col="black")
    text(11,.11,"df=10", cex=1.5)
    points(x,dchisq(x,20),type='l',lwd=3, col="black")
    text(22,.08,"df=20", cex=1.5)
    axis(1)
    par(opar)


###################################################
### chunk number 4: FreqChisqHomogeneous eval=FALSE
###################################################
## chisq.test(c(C1,C2,..))
## #OR
## chisq.test(data.xtab)


###################################################
### chunk number 5: FreqGoodnessTest eval=FALSE
###################################################
## ks.test(DV,DIST,...)
## #OR
## ks.test(DV,"dist",...)
## #For example
## ks.test(DV,"pnorm",mean(DV), sd(DV))


###################################################
### chunk number 6: FreqContingAssumpt eval=FALSE
###################################################
## chisq.test(data.xtab, corr=F)$exp


###################################################
### chunk number 7: FreqContingHomogeneous eval=FALSE
###################################################
## chisq.test(data.xtab, corr=F)


###################################################
### chunk number 8: FreqHomogeneityTest eval=FALSE
###################################################
## fisher.test(data.xtab)


###################################################
### chunk number 9: FreqOddsRatio eval=FALSE
###################################################
## library(biology)
## oddsratios(data.xtab)


###################################################
### chunk number 10: FreqOddsRatio eval=FALSE
###################################################
## library(vcd)
## strucplot(data.xtab, shade=T)


###################################################
### chunk number 11: SeedsReadTable
###################################################
COUNT <- c(152,39,53,6)
TYPE <-gl(4,1,4,c("YellowSmooth", "YellowWrinkled", "GreenSmooth", 
"GreenWrinkled"))
seeds <- data.frame(TYPE,COUNT)


###################################################
### chunk number 12: SeedsXtabs
###################################################
seeds.xtab <- xtabs(COUNT~TYPE,seeds)


###################################################
### chunk number 13: SeedsChisqEXP
###################################################
chisq.test(seeds.xtab, p=c(9/16,3/16,3/16,1/16), correct=F)$exp


###################################################
### chunk number 14: SeedsChisq
###################################################
chisq.test(seeds.xtab,p=c(9/16,3/16,3/16,1/16), correct=F)


###################################################
### chunk number 15: BeansReadTable
###################################################
COUNT<-c(63,31,28,12,39,16,40,12)
PHENOTYPE <- gl(8,1,8, c("Pt", "Pt", "Rb", "Rt", "P", "O", "B", "T"))
beans <- data.frame(PHENOTYPE,COUNT)


###################################################
### chunk number 16: BeansXtabs
###################################################
beans.xtab <- xtabs(COUNT~PHENOTYPE,beans)


###################################################
### chunk number 17: BeansProbs
###################################################
H0 <- c(18,6,6,2,12,4,12,4)
H0.prob <- H0/sum(H0)


###################################################
### chunk number 18: BeansExp
###################################################
library(biology)
g.test(beans.xtab,p=H0.prob)$exp


###################################################
### chunk number 19: BeansGtest
###################################################
g.test(beans.xtab,p=H0.prob,correct="williams")


###################################################
### chunk number 20: RobertsReadTable
###################################################
roberts <- read.table('roberts.csv', header=T, sep=',')


###################################################
### chunk number 21: RobertsTab
###################################################
roberts.xtab <- table(roberts$POSITION,roberts$DEAD)
#OR
roberts.xtab <- with(roberts,table(POSITION,DEAD))
roberts.xtab


###################################################
### chunk number 22: RobertsChisq
###################################################
chisq.test(roberts.xtab,corr=F)$exp


###################################################
### chunk number 23: RobertsChisq
###################################################
chisq.test(roberts.xtab,corr=F)
library(biology)
g.test(roberts.xtab,corr="williams")


###################################################
### chunk number 24: RobertsResiduals
###################################################
chisq.test(roberts.xtab,corr=F)$res


###################################################
### chunk number 25: RobertsOddsRatio
###################################################
# compare each of the positions pairwise to the bottom position
#add a small constant to remove the 0 value
library(biology)
oddsratios(roberts.xtab+.5)


###################################################
### chunk number 26: RobertsMosaicPlot
###################################################
getOption("SweaveHooks")[["fig"]]()
library(vcd)
strucplot(roberts.xtab, shade=T, labeling_args=list(
set_varnames=c(POSITION="Transect position", DEAD="Dead coolibah trees"),
offset_varnames = c(left = 1.5, top=1.5)), margins=c(5,2,2,5))


###################################################
### chunk number 27: SnailsH0
###################################################
H0.tab <- matrix(c(0.7*0.4,0.7*0.6,0.3*0.4,0.3*0.6),nrow=2)
rownames(H0.tab) <- c("Aust","Bemb")
colnames(H0.tab) <- c("Empty","Occupied")
library(epitools)
table.margins(H0.tab)


###################################################
### chunk number 28: SnailsAltHypo
###################################################
HA.tab <- matrix(c(0.7*0.4,0.7*0.6,0.3*0.5,0.3*0.5),nrow=2)
rownames(HA.tab) <- c("Aust","Bemb")
colnames(HA.tab) <- c("Empty","Occupied")
table.margins(HA.tab)


###################################################
### chunk number 29: SnailsEffectSize
###################################################
ws <- sqrt(chisq.test(as.vector(HA.tab), p=as.vector(H0.tab))$stat[[1]])


###################################################
### chunk number 30: SnailsPwr
###################################################
library(pwr)
pwr.chisq.test(df=1, w=ws, power=0.8)


###################################################
### chunk number 31: CreateRFile
###################################################
Stangle("../../frequencyAnalysis.rnw")


