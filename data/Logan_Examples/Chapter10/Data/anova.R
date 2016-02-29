###################################################
### chunk number 1: Anova_SetDir
###################################################
setwd("Anova/Data")
setCacheDir("../cache")


###################################################
### chunk number 2: AnovaLayout
###################################################
library(sp)
set.seed(75)
op<- par(mar=c(0.1,0.1,0.1,0.1))
plot(c(0,1),c(0,1),xlim=c(0,100), ylim=c(0,100), xlab="", ylab="",type="n", axes=F)
pointer <- xy.coords(x=runif(100,0,100), y=runif(100,0,100))	
points(pointer, col="gray", pch="^")
set.seed(75)
treat1 <- xy.coords(x=runif(4,0,90), y=runif(4,0,90))
treat2 <- xy.coords(x=runif(4,0,90), y=runif(4,0,90))
treat3 <- xy.coords(x=runif(4,0,90), y=runif(4,0,90))
treat4 <- xy.coords(x=runif(4,0,90), y=runif(4,0,90))

rect(treat1$x, treat1$y, treat1$x+10, treat1$y+10, col="gray60", density=NA)
text(treat1$x+5, treat1$y+5, "1", color="white", font=2)
#point(treat1$x+5, treat1$y+5, pch="1", color="yellow")

rect(treat2$x, treat2$y, treat2$x+10, treat2$y+10, col="black", density=20)
text(treat2$x+5, treat2$y+5, "2", font=2)

rect(treat3$x, treat3$y, treat3$x+10, treat3$y+10, col="gray80", density=NA)
text(treat3$x+5, treat3$y+5, "3", font=2)

rect(treat4$x, treat4$y, treat4$x+10, treat4$y+10)
text(treat4$x+5, treat4$y+5, "4", font=2)

box(bty="o", col="gray")
par(op)


###################################################
### chunk number 3: AnovaDiagram1
###################################################
dv <- c(8,6,5,1,2.5,3)
factor <- factor(c('A','A','A','B','B','B'))
factor1 <- c(1,2,3,5,6,7)
factor1 <- c(1-.5,2-.5,3-.5,5,6,7)
mean.a <- mean(dv[1:3])
mean.b <- mean(dv[4:6])
mean.ab <- mean(dv)

old <- par(mfrow=c(2,2),mar=c(2,1,0.1,0.1),xpd=NA)
#raw plot with means
plot(factor1,dv,pch=16,cex=1,axes=F,xlab='',ylab='',ylim=c(0,10),xlim=c(0,8))
axis(1, at=c(2,6),lab=c('Group A','Group B'))
axis(2,labels=F)
lines(factor1[1:3],rep(mean(dv[1:3]),3), col="black",lwd=2)
#put in the mean of factor B
lines(factor1[4:6],rep(mean(dv[4:6]),3),col="black",lwd=2)
#put in the overall mean (constant)
lines(factor1,rep(mean(dv),6), col="black",lwd=2)
text(4,7.5,'Group A mean',pos=4)
arrows(2.65,6.5,3.9,7.5,code=1, length=.1)
text(0,3,'Overal mean',pos=4)
arrows(2.5,3,3.9,4,code=2, length=.1)
text(1,1,'Group B mean',pos=4)
arrows(4,1,5.5,2,code=2, length=.1)
text(0,10,'a',font=2,cex=2)
box()

#plot with explained bit
plot(factor1,dv,pch=16,cex=1,axes=F,xlab='',ylab='',ylim=c(0,10),xlim=c(0,8))
#text(2,-1,'Group A')
#text(6,-1,'Group B')
axis(1, at=c(2,6),lab=c('Group A','Group B'))
axis(2,labels=F)
text(0,10,'b',font=2,cex=2)
box()

#put in the mean of factor A
lines(factor1[1:3],rep(mean(dv[1:3]),3), col="black",lwd=2)
#put in the mean of factor B
lines(factor1[4:6],rep(mean(dv[4:6]),3),col="black",lwd=2)
#put in the overall mean (constant)
lines(factor1,rep(mean(dv),6), col="black",lwd=2)

#explained components
arrows(factor1, c(rep(mean.a,3), rep(mean.b,3)),factor1, c(rep(mean.ab,3), rep(mean.ab,3)),lwd=1,lty=1, code=3,length=.1, col='gray')
points(factor1,dv,pch=16,cex=1,col='black')

#rect(1.5,7,8.3,10.4,col='white')
#text(1.5,9.75,expression(paste(SS[groups], "= sum of squared explained lines")),pos=4)
#text(1.5,9,expression(paste(MS[groups], "= conservative mean var explained")),pos=4)
#text(2.5,7.7,expression(paste("= ", over(SS[groups],df[groups]))),pos=4)

rect(3,5,8.3,10.4,col='white')
text(3,9.75,expression(paste(SS[groups], "= sum of squared")),pos=4)
text(4.5,9.25,expression(paste("explained distances")),pos=4)
text(3,8,expression(paste(MS[groups], "= conservative mean")),pos=4)
text(4.5,7.5,expression(paste("var explained")),pos=4)
text(4.5,6,expression(paste("= ", over(SS[groups],df[groups]))),pos=4)

text(1,1,'Explained var.\n(distances)',pos=4)
arrows(3.5,1.5,4.9,3,code=2, length=.1)
arrows(3.5,1.5,1.7,5,code=2, length=.1)

#plot with unexplained
plot(factor1,dv,pch=16,cex=1,axes=F,xlab='TREATMENTS',ylab='',ylim=c(0,10),xlim=c(0,8))
#put in the mean of factor A
lines(factor1[1:3],rep(mean(dv[1:3]),3), col="black",lwd=2)
#put in the mean of factor B
lines(factor1[4:6],rep(mean(dv[4:6]),3),col="black",lwd=2)
#put in the overall mean (constant)
lines(factor1,rep(mean(dv),6), col="black",lwd=2)

#unexplained components
#segments(factor1, dv ,factor1, c(rep(mean.a,3), rep(mean.b,3)),lwd=2,lty=3)
arrows(factor1, dv ,factor1, c(rep(mean.a,3), rep(mean.b,3)),lwd=1,lty=1, code=3,length=.1,col='gray')
points(factor1,dv,pch=16,cex=1, col='black')
#text(2,-1,'Group A')
#text(6,-1,'Group B')
axis(1, at=c(2,6),lab=c('Group A','Group B'))
axis(2,labels=F)
text(0,10,'c',font=2,cex=2)
box()

#text(0,9.75,expression(paste(SS[residual], "= sum of squared unexplained lines")),pos=4)
#text(0,9,expression(paste(MS[residual], "= conservative mean var unexplained")),pos=4)
#text(1.4,8,expression(paste("= ", over(SS[residuals],df[residuals]))),pos=4)

rect(3,5,8.3,10.4,col='white')
text(3,9.75,expression(paste(SS[residual], "= sum of squared")),pos=4)
text(4.5,9.25,expression(paste("unexplained distances")),pos=4)
text(3,8,expression(paste(MS[residual], "= conservative mean")),pos=4)
text(4.5,7.5,expression(paste("var unexplained")),pos=4)
text(4.5,6,expression(paste("= ", over(SS[residual],df[residual]))),pos=4)

text(.5,1,'Unexplained var.\n(distances)',pos=4)
arrows(3.5,1.5,4.9,1.5,code=2, length=.1)
arrows(3.5,1.5,2.6,5.5,code=2, length=.1)

#f-distribution
#plot(factor1,dv,pch=16,cex=2,axes=F,xlab='TREATMENTS',ylab='',ylim=c(0,10),xlim=c(0,8))
plot(seq(0,5,length=100), df(seq(0,5,length=100), df1=3, df2=10),type='l',xlab='', ylab='',axes=F,ylim=c(0,1))
#axis(1)
lines(c(0,5),c(0,0), col='gray')
box()
rect(1,.85,5.2,1.04,col='white')
text(1.25,.92,expression(paste("F-ratio=",over(Explained,Unexplained), " = ",over(MS[groups], MS[residual]))),pos=4)
text(2,.5,expression(paste('F-distribution\n(Distribution of all possible\nexpected F-ratios when the')),pos=4)
text(2,.44,expression(paste(H[0],' is true)')),pos=4)
#text(3,.5,'Distribution of all possible\nexpected F-ratios when the H0 is true',pos=4)
arrows(2,.5,1.5,.4,code=2,length=.1)
text(0,1,'d',font=2,cex=2)
par(old)


###################################################
### chunk number 4: AnovaKey1 eval=FALSE
###################################################
## boxplot(DV~Factor, dataset)


###################################################
### chunk number 5: AnovaKey2 eval=FALSE
###################################################
## plot(tapply(dataset$DV, dataset$Factor, var), tapply(dataset$DV,
## dataset$Factor, mean))


###################################################
### chunk number 6: AnovaKey3 eval=FALSE
###################################################
## data.aov <- aov(DV~Factor, dataset)
## plot(data.aov)
## anova(data.aov)


###################################################
### chunk number 7: AnovaKey4 eval=FALSE
###################################################
## anova(aov(DV~Factor, dataset))


###################################################
### chunk number 8: AnovaKey5 eval=FALSE
###################################################
## library(nlme)
## #Using  maximum likelihood (ML) variance estimates
## data.lme <- lme(DV~1, random=~1|Factor, data=dataset, method='ML')
## VarCorr(data.lme)
## #OR using  restricted maximum likelihood (REML) variance estimates
## data.lme <- lme(DV~1, random=~1|Factor, data=dataset, method='REML')
## VarCorr(data.lme)


###################################################
### chunk number 9: AnovaKey6 eval=FALSE
###################################################
## contrasts(dataset$Factor) <- cbind(c(contrasts), c(contrasts), ...)
## round(crossprod(contrasts(dataset$Factor)),2)
## data.list <- list(Factor = list('lab'=1, ..), ..)
## data.aov <- aov(DV~Factor, data=dataset)
## plot(data.aov)
## summary(data.aov, split=data.list)


###################################################
### chunk number 10: AnovaKey7 eval=FALSE
###################################################
## contrasts(dataset$Factor) <- 'contr.poly'
## data.list <- list(Factor = list('Linear'=1))
## data.aov <- aov(DV~Factor, data=dataset)
## plot(data.aov)
## summary(data.aov, split=data.list)


###################################################
### chunk number 11: AnovaKey8 eval=FALSE
###################################################
## oneway.test(DV~Factor, var.equal=F)


###################################################
### chunk number 12: AnovaKey9 eval=FALSE
###################################################
## kruskal.test(DV~Factor, var.equal=F)


###################################################
### chunk number 13: AnovaKey10 eval=FALSE
###################################################
## library(boot)
## data.boot <- boot(dataset, stat, R=999, sim="parametric", rand.gen=rand.gen)
## plot(data.boot)
## print(data.boot)


###################################################
### chunk number 14: AnovaKey11 eval=FALSE
###################################################
## library(multcomp)
## summary(glht(model, linfct=mcp(Factor="Tukey")))


###################################################
### chunk number 15: AnovaKey11 eval=FALSE
###################################################
## library(npmc)
## data <- data.frame(var=dataset$DV, class=dataset$Factor)
## summary(npmc(data), type='steel')


###################################################
### chunk number 16: AnovaKey12 eval=FALSE
###################################################
## library(multtest)
## mt.rawp2adjp(pvalues, proc='SidakSD')
## #OR
## p.adjust(pvalues, method='holm')


###################################################
### chunk number 17: AnovaKey13 eval=FALSE
###################################################
## pairwise.t.test(DV~Factor, pool.sd=F, p.adjust='holm')


###################################################
### chunk number 18: AnovaKey14 eval=FALSE
###################################################
## pairwise.wilcox.test(DV~Factor, p.adjust='holm')


###################################################
### chunk number 19: MedleyReadTable
###################################################
medley <- read.table('medley.csv', header=T, sep=',')


###################################################
### chunk number 20: MedleyFactor
###################################################
medley$ZINC <- factor(medley$ZINC, levels=c('HIGH', 'MED', 'LOW', 'BACK'), ordered=F)


###################################################
### chunk number 21: MedleyBoxplot
###################################################
boxplot(DIVERSITY~ZINC, medley)


###################################################
### chunk number 22: MedleyMeanvsVar
###################################################
plot(tapply(medley$DIVERSITY, medley$ZINC, mean),
tapply(medley$DIVERSITY, medley$ZINC, var))


###################################################
### chunk number 23: MedleyResidPlot
###################################################
op<- par(mfrow=c(2,2))
medley.aov <- aov(DIVERSITY~ZINC, medley)
plot(medley.aov)
par(op)


###################################################
### chunk number 24: MedleyResidPlot eval=FALSE
###################################################
## medley.aov <- aov(DIVERSITY~ZINC, medley)
## plot(medley.aov)


###################################################
### chunk number 25: MedleyAnova
###################################################
anova(medley.aov)


###################################################
### chunk number 26: MedleyTukeys
###################################################
library(multcomp)
summary(glht(medley.aov, linfct=mcp(ZINC='Tukey')))


###################################################
### chunk number 27: MedleyBargraph
###################################################
library(biology)
Mbargraph(medley$DIVERSITY, medley$ZINC,symbols=c('A','AB','B','AB'),
ylab="Mean diatom diversity", xlab="Zinc concentration")


###################################################
### chunk number 28: KeoughReadTable
###################################################
keough <- read.table('keough.csv', header=T, sep=',')


###################################################
### chunk number 29: TEMP
###################################################
op <- options(width=40)


###################################################
### chunk number 30: keoughBoxplot1
###################################################
boxplot(SERP~BIOFILM, data=keough)


###################################################
### chunk number 31: keoughMeanvsVar1
###################################################
with(keough, plot(tapply(SERP, BIOFILM, 
mean), tapply(SERP, BIOFILM, var)))


###################################################
### chunk number 32: keoughBoxplot2
###################################################
boxplot(log10(SERP)~BIOFILM, 
data=keough)


###################################################
### chunk number 33: keoughMeanvsVar2
###################################################
with(keough, plot(tapply(log10(SERP), 
BIOFILM, mean), tapply(log10(SERP), 
BIOFILM, var)))


###################################################
### chunk number 34: TEMP1
###################################################
options(op)


###################################################
### chunk number 35: KeoughContrasts
###################################################
contrasts(keough$BIOFILM) <- cbind(c(0,1,0,-1), c(2,-1,0,-1), c(-1,-1,3,-1))


###################################################
### chunk number 36: KeoughCrossProd
###################################################
round(crossprod(contrasts(keough$BIOFILM)),2)


###################################################
### chunk number 37: KeoghList
###################################################
keough.list <- list(BIOFILM=list("NL vs UL"=1, "F vs (NL&UL)"=2, 
"SL vs (F&NL&UL)"=3))


###################################################
### chunk number 38: KeoughAOV
###################################################
keough.aov <- aov(log10(SERP)~BIOFILM, data=keough)


###################################################
### chunk number 39: keoughResidPlot
###################################################
op<-par(mfrow=c(2,2))
plot(keough.aov)
par(op)


###################################################
### chunk number 40: keoughResidPlot1 eval=FALSE
###################################################
## plot(keough.aov)


###################################################
### chunk number 41: KeoughSUmmary
###################################################
summary(keough.aov, split=keough.list)


###################################################
### chunk number 42: keoughBargraph
###################################################
means <- with(keough, tapply(SERP, BIOFILM, mean, na.rm =T))
sds <- with(keough, tapply(SERP, BIOFILM, sd, na.rm =T))
n <- with(keough, tapply(SERP, BIOFILM, length))
ses <- sds/sqrt(n)
ys <- pretty(c(means - ses, means + (2 * ses)))
xs<-barplot(means, beside=T, axes=F, ann=F, ylim = c(min(ys), max(ys)), xpd=F)
arrows(xs, means+ses, xs, means-ses, ang=90, length=0.1, code=3)
axis(2, las = 1)
mtext(2, text = "Mean number of serpulids", line = 3, cex = 1.5)
mtext(1, text = "Biofilm treatment", line = 3, cex = 1.5)
box(bty="l")


###################################################
### chunk number 43: Keough1ReadTable
###################################################
keough <- read.table('keough.csv', header=T, sep=',')


###################################################
### chunk number 44: Keough1Factor
###################################################
keough$BIOFILM <- factor(keough$BIOFILM, levels=c('SL', 'NL', 'UL', 'F'))


###################################################
### chunk number 45: Keough1Contrasts
###################################################
contrasts(keough$BIOFILM) <-'contr.poly'


###################################################
### chunk number 46: Keough1List
###################################################
keough.list <- list(BIOFILM=list("Linear"=1, "Quadratic"=2, "Cubic"=3))


###################################################
### chunk number 47: Keough1Anova
###################################################
keough.aov <- aov(log10(SERP)~BIOFILM, data=keough)


###################################################
### chunk number 48: Keough1Summary
###################################################
summary(keough.aov, split=keough.list)


###################################################
### chunk number 49: Medley1ReadTable
###################################################
medley <- read.table('medley.csv', header=T, sep=',')


###################################################
### chunk number 50: Medley1Boxplot
###################################################
boxplot(DIVERSITY~STREAM, medley)


###################################################
### chunk number 51: Medley1AOV
###################################################
medley.aov <- aov(DIVERSITY~STREAM, medley)
anova(medley.aov)


###################################################
### chunk number 52: MedleyVarCorr
###################################################
library(nlme)
print(VarCorr(lme(DIVERSITY~1,  random=~1 | STREAM, method="ML", data=medley)))
print(VarCorr(lme(DIVERSITY~1,  random=~1 | STREAM, method="REML", data=medley)))


###################################################
### chunk number 53: PurvesReadTable
###################################################
purves <- read.table('purves.csv', header=T, sep=',')


###################################################
### chunk number 54: purvesBoxplot
###################################################
boxplot(LENGTH~TREAT, data=purves)


###################################################
### chunk number 55: purvesKW
###################################################
kruskal.test(LENGTH~TREAT, data=purves)


###################################################
### chunk number 56: PurveysNPMC1
###################################################
library(npmc)


###################################################
### chunk number 57: PurveysNPMCcache
###################################################
dat <- data.frame(var=purves$LENGTH, class=purves$TREAT)
ss<-summary(npmc(dat),type='Steel')


###################################################
### chunk number 58: PurveysNPMC2 eval=FALSE
###################################################
## library(npmc)
## # randomization process is slow and values differ slightly each time
## dat <- data.frame(var=purves$LENGTH, class=purves$TREAT)
## summary(npmc(dat),type='Steel')


###################################################
### chunk number 59: PurverysNPMC3
###################################################
ss


###################################################
### chunk number 60: purvesBargraph
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


###################################################
### chunk number 61: SanchezReadTable
###################################################
sanchez <- read.table('sanchez.csv', header=T, sep=',')


###################################################
### chunk number 62: Options
###################################################
op <- options(width=40)


###################################################
### chunk number 63: sanchez1Boxplot
###################################################
boxplot(GUANO~COLTYPE, data=sanchez)


###################################################
### chunk number 64: sanchez2Boxplot
###################################################
boxplot(sqrt(GUANO)~COLTYPE, data=sanchez)


###################################################
### chunk number 65: Options
###################################################
options(op)


###################################################
### chunk number 66: sanchezOneWay
###################################################
oneway.test(sqrt(GUANO)~COLTYPE, data=sanchez)


###################################################
### chunk number 67: SanchezHolm
###################################################
pairwise.t.test(sqrt(sanchez$GUANO), sanchez$COLTYPE, pool.sd=F, p.adj="holm")


###################################################
### chunk number 68: SanchezNone
###################################################
pvalues <- pairwise.t.test(sqrt(sanchez$GUANO), sanchez$COLTYPE, pool.sd=F, 
p.adj="none")$p.value
pvalues


###################################################
### chunk number 69: SanchezSidakSD
###################################################
library(multtest)
mt.rawp2adjp(pvalues,proc="SidakSD")


###################################################
### chunk number 70: sanchezBargraph
###################################################
library(biology)
Mbargraph(sanchez$GUANO, sanchez$COLTYPE, symbols=c('A','B','A'),
ylab="Mean percentage Guano cover", xlab="Bird colony type")


###################################################
### chunk number 71: AntsReadTable
###################################################
ants <- read.table('ants.csv', header=T, sep=',')


###################################################
### chunk number 72: antsBoxplot1
###################################################
boxplot(BIOMASS~MONTH, ants)


###################################################
### chunk number 73: antsBoxplot2
###################################################
boxplot(BIOMASS^(1/3)~MONTH, ants)


###################################################
### chunk number 74: AntsStat
###################################################
stat <- function(data, indices) {
f.ratio <- anova(aov(BIOMASS^(1/3)~MONTH, data))$"F value"[1]
f.ratio
}


###################################################
### chunk number 75: AntsRand
###################################################
rand.gen <- function(data,mle) {
out <- data
out$MONTH <- sample(out$MONTH, replace=F)
out
}


###################################################
### chunk number 76: AntsBoot1
###################################################
library(boot)


###################################################
### chunk number 77: AntsBootcache
###################################################
ants.boot <- boot(ants, stat, R=5000, sim="parametric", ran.gen=rand.gen)


###################################################
### chunk number 78: AntsBoot2 eval=FALSE
###################################################
## ants.boot <- boot(ants, stat, R=5000, sim="parametric", ran.gen=rand.gen)


###################################################
### chunk number 79: antsDist1
###################################################
plot(ants.boot)


###################################################
### chunk number 80: AntsPrint
###################################################
print(ants.boot)


###################################################
### chunk number 81: AntsP
###################################################
f <- length(ants.boot[ants.boot$t >= ants.boot$t0])+1
print(f/(ants.boot$R + 1))


###################################################
### chunk number 82: AntsBoot1cache
###################################################
#compare September and August
ants.rand1 <- boot(ants[ants$MONTH=='September' | ants$MONTH=='August',],
stat, R=1000, sim="parametric", ran.gen=rand.gen)
ants.rand2 <- boot(ants[ants$MONTH=='September' | ants$MONTH=='July',], 
stat, R=1000, sim="parametric", ran.gen=rand.gen)
ants.rand3 <- boot(ants[ants$MONTH=='September' | ants$MONTH=='June',], 
stat, R=1000, sim="parametric", ran.gen=rand.gen)
p.S.Jn <-print(length(ants.rand3[ants.rand3$t >= ants.rand3$t0])/
(ants.rand3$R+ 1))
ants.rand4 <- boot(ants[ants$MONTH=='August' | ants$MONTH=='July',], 
stat, R=1000, sim="parametric", ran.gen=rand.gen)
p.A.Jy <-print(length(ants.rand4[ants.rand4$t >= ants.rand4$t0])/
(ants.rand4$R+ 1))
ants.rand5 <- boot(ants[ants$MONTH=='August' | ants$MONTH=='June',], 
stat, R=1000, sim="parametric", ran.gen=rand.gen)
p.A.Jn <-print(length(ants.rand5[ants.rand5$t >= ants.rand5$t0])/
(ants.rand5$R+ 1))
ants.rand6 <- boot(ants[ants$MONTH=='August' | ants$MONTH=='June',], 
stat, R=1000, sim="parametric", ran.gen=rand.gen)
p.Jy.Jn <-print(length(ants.rand6[ants.rand6$t >= ants.rand6$t0])/
(ants.rand6$R+ 1))


###################################################
### chunk number 83: AntsBoot1a eval=FALSE
###################################################
## #compare September and August
## ants.rand1 <- boot(ants[ants$MONTH=='September' | ants$MONTH=='August',],
## stat, R=1000, sim="parametric", ran.gen=rand.gen)
## ants.rand2 <- boot(ants[ants$MONTH=='September' | ants$MONTH=='July',], 
## stat, R=1000, sim="parametric", ran.gen=rand.gen)


###################################################
### chunk number 84: AntsBoot1b
###################################################
#compare September and August
p.S.A <-print(length(ants.rand1[ants.rand1
$t >= ants.rand1$t0])/
(ants.rand1$R + 1))
#compare September and July
p.S.Jy <-print(length(ants.rand2[ants.rand2$t >= ants.rand2$t0])/
(ants.rand2$R+ 1))


###################################################
### chunk number 85: AntsPvalues
###################################################
p.values <- c('Sep vs Aug'=p.S.A, 'Sep vs Jul'=p.S.Jy, 'Sep vs Jun'=p.S.Jn, 
'Aug vs Jul'=p.A.Jy, 'Aug vs Jun'=p.A.Jn, 'Jul vs Jun'=p.Jy.Jn)
p.adjust(p.values,'holm')


###################################################
### chunk number 86: antsBargraph
###################################################
Mbargraph(ants$BIOMASS, ants$MONTH, symbols=c('A','AB','AB','B'),
ylab="Mean ant biomass", xlab="Month")


###################################################
### chunk number 87: CreateRFile
###################################################
Stangle("../../anova.rnw")


