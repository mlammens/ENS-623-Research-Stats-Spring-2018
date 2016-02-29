###################################################
### chunk number 1: Nested_SetDir
###################################################
setwd("NestedAnova/Data")
setCacheDir("../cache")


###################################################
### chunk number 2: nestedDiagram1-singleFactor
###################################################
library(sp)
set.seed(2)
op<- par(mar=c(0,0,1,0))
plot(c(0,1),c(0,1),xlim=c(0,100), ylim=c(0,100), xlab="", ylab="",type="n", axes=F)
sx<-seq(5,100-30,l=3)
sy<-seq(5,100-45,l=2)
dens<-sample(c(20,20,20,0,0,0))
#treat <- ifelse(dens>0, "B", "A")
treat <- ifelse(dens>0, "", "")
col<-c("black", "gray40", "gray40")
ln<-c(1,2,2)
ll<-0
xl<-vector("numeric",length(sx)*length(sy))
yl<-vector("numeric",length(sx)*length(sy))
for (i in 1:length(sx)) {
  for (j in 1:length(sy)){
	ll<-ll+1
    	rect(sx[i], sy[j],sx[i]+30, sy[j]+45, density=0, col="gray")
	pol.x<-c(sx[i],sx[i]+30,sx[i]+30,sx[i],sx[i])
	pol.y<-c(sy[j],sy[j],sy[j]+45,sy[j]+45,sy[j])
	pointer <- xy.coords(x=runif(15,sx[i],sx[i]+30), y=runif(15,sy[j],sy[j]+45))	
	points(pointer, col="gray", pch="^")
	for (pt in 1:(dens[ll]*.5)+5) {
		pointer1 <- xy.coords(x=rnorm(10,pointer$x[pt],3), y=rnorm(10,pointer$y[pt],3))
		pointer$x<-c(pointer$x,pointer1$x)
		pointer$y<-c(pointer$y,pointer1$y)	
		
	}	
	pts<-point.in.polygon(pointer$x,pointer$y, pol.x,pol.y)
	points(pointer$x[pts>0],pointer$y[pts>0], col="gray", pch="^")

        for (k in 1:1){
          fin<-T
          while (fin) {
            xx<-runif(1,sx[i],sx[i]+30)
            xl[ll]<-xx
            yy<-runif(1,sy[j],sy[j]+45)
            yl[ll]<-yy
            if (all(point.in.polygon(c(xx,xx+5),c(yy,yy+5),c(sx[i],sx[i]+30,sx[i]+30,sx[i],sx[i]), c(sy[j],sy[j],sy[j]+45,sy[j]+45,sy[j]))>0)) {
              rect(xx, yy, xx+5, yy+5, lty=ln[k], col=col[k], density=dens[ll])
              text(xx+2.5, yy+2.5, treat[ll], col=col[k])
              fin<-F
            }			
          }
        }
      }
}
text(0,100, "a)", font=2, cex=1.5)
par(op)


###################################################
### chunk number 3: nestedDiagram1-nested
###################################################
set.seed(5)
op<- par(mar=c(0,0,1,0))
plot(c(0,1),c(0,1),xlim=c(0,100), ylim=c(0,100), xlab="", ylab="",type="n", axes=F)
sx<-seq(5,100-30,l=3)
sy<-seq(5,100-45,l=2)
#dens<-sample(c(20,20,20,0,0,0))
#treat <- ifelse(dens>0, "B", "A")
ll<-0
for (i in 1:length(sx)) {
  for (j in 1:length(sy)){
	ll<-ll+1
    rect(sx[i], sy[j],sx[i]+30, sy[j]+45, density=dens[ll], col="gray")
    text(sx[i]+15, sy[j]+2.5, paste("Nest",ll))
	#text(sx[i]+15,sy[j]+45+2.5,paste("Treat",treat[ll]))
        for (k in 1:4){
          fin<-T
          while (fin) {
            xx<-ifelse(k==1,xl[ll],runif(1,sx[i],sx[i]+30))
            yy<-ifelse(k==1,yl[ll],runif(1,sy[j],sy[j]+45))
            if (all(point.in.polygon(c(xx,xx+5),c(yy,yy+5),c(sx[i],sx[i]+30,sx[i]+30,sx[i],sx[i]), c(sy[j],sy[j],sy[j]+45,sy[j]+45,sy[j]))>0)) {
              rect(xx, yy, xx+5, yy+5)
              text(xx+2.5, yy+2.5, treat[ll])
              fin<-F
            }			
          }
        }
   }
}
text(0,100, "b)", font=2, cex=1.5)
arrows(52.5,-.5,sx[1]+15,sy[1]+1, length=.05)
arrows(52.5,-.5,sx[2]+15,sy[1]+1, length=.05)
arrows(52.5,-.5,sx[3]+15,sy[1]+1, length=.05)
text(52.5,-2,"Factor B (nesting factor)", font=2)
text(52.5,102,"Factor A (Main treatment factor) -", font=2)
rect(78, 101,78+3, 101+3, density=20, col="gray")
rect(78, 101,78+3, 101+3)
text(83,102," or ", font=2)
rect(85, 101,85+3, 101+3)
par(op)


###################################################
### chunk number 4: NAKey1 eval=FALSE
###################################################
## data.B.agg <- with(data, aggregate(data.frame(DV),
## by = list(A = A, B = B), mean))
## #OR
## library(nlme)
## data.B.agg <- gsummary(data, data$B)
## boxplot(DV ~ A, data.B.agg)


###################################################
### chunk number 5: NAKey2 eval=FALSE
###################################################
## library(nlme)
## data.C.agg <- gsummary(data,data$C)
## boxplot(DV~A:B, data.C.agg)


###################################################
### chunk number 6: NAKey3 eval=FALSE
###################################################
## boxplot(DV~A:B, data)


###################################################
### chunk number 7: NAKey4 eval=FALSE
###################################################
## with(data.B.agg, plot(tapply(DV, A, var), tapply(DV, A, mean)))


###################################################
### chunk number 8: NAK5 eval=FALSE
###################################################
## library(biology)
## is.balanced(DV~A+b+C+.., data)
## #OR
## !is.list(replications(DV~A+b+C+.., data))


###################################################
### chunk number 9: NAK6 eval=FALSE
###################################################
## replications(DV~A+b+C+.., data)


###################################################
### chunk number 10: NAK7 eval=FALSE
###################################################
## data.aov <- aov(DV~A+Error(B), data)
## summary(data.aov)


###################################################
### chunk number 11: NAK8 eval=FALSE
###################################################
## library(nlme)
## data.B.agg <- gsummary(data,data$B)


###################################################
### chunk number 12: NAK9 eval=FALSE
###################################################
## anova(aov(DV~A, data.B.agg))


###################################################
### chunk number 13: NAK10 eval=FALSE
###################################################
## library(nlme)
## data.C.agg <- gsummary(data,data$B)


###################################################
### chunk number 14: NAK11 eval=FALSE
###################################################
## anova(aov(DV~A+B, data.C.agg))


###################################################
### chunk number 15: NAK12 eval=FALSE
###################################################
## library(nlme)
## data.lme <- lme(DV~A, random=~1|B, data)
## summary(data.lme)
## anova(data.lme)


###################################################
### chunk number 16: NAK13 eval=FALSE
###################################################
## data.lme <- lme(DV~A, random=~1|B/C, data)
## summary(data.lme)
## anova(data.lme)


###################################################
### chunk number 17: NAK14 eval=FALSE
###################################################
## library(lme4)
## data.lmer <- lmer(DV~A+(1|B), data)
## summary(data.lmer)
## anova(data.lmer)


###################################################
### chunk number 18: NAK14 eval=FALSE
###################################################
## data.lmer <- lmer(DV~A+(1|B/C), data)
## summary(data.lmer)
## anova(data.lmer)


###################################################
### chunk number 19: NAK16 eval=FALSE
###################################################
## library(languageR)
## pvals.fnc(data.lmer)


###################################################
### chunk number 20: NAK16a eval=FALSE
###################################################
## library(languageR)
## pvals<-pvals.fnc(data.lmer, nsim=10000, withMCMC=T)
## library(biology)
## mcmcpvalue(as.matrix(pvals$mcmc),"A")


###################################################
### chunk number 21: NAK17 eval=FALSE
###################################################
## plot(resid(model[[2]])~fitted(model[[2]]))


###################################################
### chunk number 22: NAK18 eval=FALSE
###################################################
## plot(resid(model)~fitted(model))


###################################################
### chunk number 23: NAK19 eval=FALSE
###################################################
## library(nlme)
## VarCorr(lme(lme(DV~A, random=~1|B, data)))


###################################################
### chunk number 24: NAK20 eval=FALSE
###################################################
## data.B.agg <- gsummary(data, data$B)
## oneway.test(DV~A, data.B.agg, var.equal=F)


###################################################
### chunk number 25: NAK21 eval=FALSE
###################################################
## data.B.agg <- gsummary(data, data$B)
## kruskal.test(DV~A, data.B.agg, var.equal=F)


###################################################
### chunk number 26: NAK22 eval=FALSE
###################################################
## data.B.agg <- gsummary(data, data$B)


###################################################
### chunk number 27: AndrewReadTable
###################################################
andrew <- read.table('andrew.csv', header=T, sep=',')


###################################################
### chunk number 28: AndrewClass
###################################################
class(andrew$PATCH)


###################################################
### chunk number 29: AndrewClass1
###################################################
andrew$PATCH <-factor(andrew$PATCH)
class(andrew$PATCH)


###################################################
### chunk number 30: AndrewLevels
###################################################
levels(andrew$TREAT)


###################################################
### chunk number 31: AndrewFactor
###################################################
andrew$TREAT<-factor(andrew$TREAT, levels=c("100%", "66%", "33%", "0%"))


###################################################
### chunk number 32: AndrewAGG
###################################################
andrew.agg <- with(andrew, aggregate(data.frame(ALGAE), by=list(TREAT=TREAT, 
PATCH=PATCH), mean))
#OR
library(nlme)
andrew.agg <- gsummary(andrew, groups=andrew$PATCH)


###################################################
### chunk number 33: andrewBoxplot1
###################################################
boxplot(ALGAE~TREAT, andrew.agg)


###################################################
### chunk number 34: AndrewReplications
###################################################
replications(ALGAE~TREAT+PATCH, andrew)


###################################################
### chunk number 35: AndrewIsBalanced
###################################################
library(biology)
is.balanced(ALGAE~TREAT+PATCH, andrew)


###################################################
### chunk number 36: AndrewContrasts
###################################################
contrasts(andrew$TREAT) <- contr.treatment


###################################################
### chunk number 37: AndrewAOV
###################################################
andrew.aov <- aov(ALGAE~TREAT+Error(PATCH), andrew)


###################################################
### chunk number 38: andrewResidualPlot1
###################################################
plot(resid(andrew.aov[[2]])~fitted(andrew.aov[[2]]))


###################################################
### chunk number 39: AndrewSummary
###################################################
summary(andrew.aov, split=list(TREAT=list('cont vs 66'=1, 'cont vs 33'=2,
'cont vs 0'=3)))


###################################################
### chunk number 40: AndrewVarCorr
###################################################
library(nlme)
VarCorr(lme(ALGAE~1,random=~1|TREAT/PATCH, andrew))


###################################################
### chunk number 41: Andrew1AGG
###################################################
andrew.patch <- with(andrew, aggregate(data.frame(ALGAE),
by=list(TREAT=TREAT, 
PATCH=PATCH), mean))
#OR
library(nlme)
andrew.patch <- gsummary(andrew, groups=andrew$PATCH)


###################################################
### chunk number 42: Andrew1Oneway
###################################################
oneway.test(ALGAE~TREAT, andrew.patch, var.equal=F)


###################################################
### chunk number 43: Andrew1Oneway2
###################################################
summary(aov(rank(ALGAE)~TREAT+Error(PATCH), andrew))


###################################################
### chunk number 44: PHReadTable
###################################################
ph <- read.table('ph.csv', header=T, sep=',')


###################################################
### chunk number 45: phBoxplot1
###################################################
library(nlme)
ph.agg <- gsummary(ph, groups=ph$SIRE)
boxplot(PH~DAM, ph.agg)


###################################################
### chunk number 46: phBoxplot2
###################################################
boxplot(PH~DAM:SIRE, ph)


###################################################
### chunk number 47: PHReps
###################################################
replications(PH ~ DAM + SIRE,data=ph)
library(biology)
is.balanced(PH ~ DAM + SIRE,data=ph)


###################################################
### chunk number 48: PHAOv1
###################################################
ph.aov <- aov(PH~DAM, ph.agg)
anova(ph.aov)


###################################################
### chunk number 49: PHAov2
###################################################
ph.aov1<-aov(PH~DAM+SIRE, data=ph)
anova(ph.aov1)


###################################################
### chunk number 50: PHlmer
###################################################
library(lme4)
ph.lmer <- lmer(PH~1+(1|DAM/SIRE), ph)
summary(ph.lmer)


###################################################
### chunk number 51: PHMCMCcache
###################################################
library(languageR)
ph.pval<-pvals.fnc(ph.lmer)


###################################################
### chunk number 52: PHMCMC1 eval=FALSE
###################################################
## library(languageR)
## pvals.fnc(ph.lmer)


###################################################
### chunk number 53: PHMCMC1
###################################################
ph.pval


###################################################
### chunk number 54: glycoReadTable
###################################################
glyco <- read.table('glyco.csv', header=T, sep=',')


###################################################
### chunk number 55: GlycoFactorOrder
###################################################
glyco$TREAT <- factor(glyco$TREAT, levels=c("Control","Compound217",
"Compound217Sugar"))


###################################################
### chunk number 56: glycoAgg
###################################################
library(nlme)
glyco.treat.agg <- gsummary(glyco, groups=glyco$RAT)


###################################################
### chunk number 57: glycoAgg2
###################################################
glyco.rat.agg <- gsummary(glyco, groups=glyco$PREP)


###################################################
### chunk number 58: glycoAgg3
###################################################
glyco.prep.agg <- gsummary(glyco, groups=glyco$READ)


###################################################
### chunk number 59: glycoBalance
###################################################
library(biology)
is.balanced(GLYCO ~ TREAT + RAT + PREP, data=glyco)


###################################################
### chunk number 60: glycoAOV
###################################################
glyco.aov <- aov(GLYCO~TREAT + Error(RAT/PREP), glyco)
summary(glyco.aov)


###################################################
### chunk number 61: glycoAov2
###################################################
glyco.rat.aov <- aov(GLYCO~TREAT + RAT + Error(PREP), glyco.rat.agg)
summary(glyco.rat.aov)


###################################################
### chunk number 62: glycoAov3
###################################################
glyco.prep.aov <- aov(GLYCO~TREAT + RAT + PREP, glyco.prep.agg)
summary(glyco.prep.aov)


###################################################
### chunk number 63: glycoLme
###################################################
library(nlme)
glyco.lme <- lme(GLYCO~TREAT,random=~1|RAT/PREP, glyco)
summary(glyco.lme)
anova(glyco.lme)


###################################################
### chunk number 64: glycoVarCorr eval=FALSE
###################################################
## library(nlme)
## VarCorr(glyco.lme)


###################################################
### chunk number 65: glycoVarCorr
###################################################
library(nlme)
nlme:::VarCorr(glyco.lme)


###################################################
### chunk number 66: glycoLmer
###################################################
library(lme4)
glyco.lmer <- lmer(GLYCO~TREAT + (1|RAT/PREP), glyco)


###################################################
### chunk number 67: phResidualPlot
###################################################
plot(resid(ph.lmer)~fitted(ph.lmer))


###################################################
### chunk number 68: glycoLMER
###################################################
glyco.lmer


###################################################
### chunk number 69: glycoMCMCcache
###################################################
library(languageR)
glyco.pval<-pvals.fnc(glyco.lmer, nsim=10000, withMCMC=T)
glyco.mcmc <- glyco.pval$mcmc


###################################################
### chunk number 70: glycoMCMC1 eval=FALSE
###################################################
## library(languageR)
## glyco.pval<-pvals.fnc(glyco.lmer,nsim=10000, withMCMC=T)


###################################################
### chunk number 71: glycoMCMC1
###################################################
# Fixed effects
glyco.pval$fixed
# Random effects
glyco.pval$random


###################################################
### chunk number 72: GlycoTreatmentP
###################################################
glyco.mcmc <- glyco.pval$mcmc
library(biology)
mcmcpvalue(as.matrix(glyco.mcmc), "TREAT")


###################################################
### chunk number 73: CreateRFile
###################################################
Stangle("../../nestedAnova.rnw")


