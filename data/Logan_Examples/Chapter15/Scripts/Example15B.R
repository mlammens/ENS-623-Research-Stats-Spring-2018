###################################################
### chunk number 1: pg 462
###################################################
constable <- read.table('constable.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 462
###################################################
library(car)
scatterplot(SUTW ~ IV|TREAT, constable)


###################################################
### chunk number 3: pg 462
###################################################
library(car)
scatterplot(SUTW ~ I(IV^(1/3))|TREAT, constable)


###################################################
### chunk number 4: pg 463
###################################################
library(lattice)
print(with(constable,xyplot(SUTW~I(IV^(1/3)),groups=TREAT, type=c("p","r"), col=1, par.settings = list(superpose.symbol = list(pch=1:3,col=1), superpose.line=list(lty=1:3)), key=list(space="right", lty=1:3, lines=T, points=T, pch=1:3, col=1, text=list(levels(TREAT))))))


###################################################
### chunk number 5: pg 463
###################################################
anova(aov(SUTW ~ I(IV^(1/3))*TREAT, constable))


###################################################
### chunk number 6: pg 463
###################################################
library(biology)
#fit model using a lm model (since must be able to extract intercept and slope)
constable.lm<-lm(SUTW~I(IV^(1/3))*TREAT, constable)
wilcox.JN(constable.lm, type="H")


###################################################
### chunk number 7: pg 464
###################################################
# fit the model and Wilcox modification of the Johnson-Newman 
constable.lm<-lm(SUTW~I(IV^(1/3))*TREAT, constable)      
WJN<-wilcox.JN(constable.lm, type="H")
# plot the base
plot(SUTW~I(IV^(1/3)),constable, type="n", ylim=c(0,.2),xlim=c(3,50)^(1/3), axes=F,xlab="",ylab="")
points(SUTW~I(IV^(1/3)),constable[constable$TREAT=="Initial",],col="black",pch=22)
lm1<-lm(SUTW~I(IV^(1/3)),constable, subset=TREAT=="Initial")
abline(lm1, col="black", lty=1)
points(SUTW~I(IV^(1/3)),constable[constable$TREAT=="Low",],col="black",pch=17)
lm2<-lm(SUTW~I(IV^(1/3)),constable, subset=TREAT=="Low")
abline(lm2, col="black", lty=4)
with(constable[constable$TREAT=="High",],text(SUTW~I(IV^(1/3)),"\\#H0844",vfont=c("serif","plain")))
lm3<-lm(SUTW~I(IV^(1/3)),constable, subset=TREAT=="High")
abline(lm3, col="black", lty=2)
axis(1,lab=c(10,20,30,40,50),at=c(10,20,30,40,50)^(1/3))
axis(2,las=1)
mtext("Initial body volume (ml)",1,line=3)
mtext("Suture width (mm)",2,line=3)
Mpar<-par(family="HersheySans", font=2)
library(biology)
# the legend.vfont function facilitates Hershey fonts
legend.vfont("topleft", c("\\#H0841 Initial","\\#H0844 High","\\#H0852 Low"), bty="n", lty=c(1,2,3), merge=F, title="Food regime", vfont=c("serif","plain"))
par(Mpar)
box(bty="l")
mn<-min(constable$IV^(1/3))
mx<-max(constable$IV^(1/3))
#since lower<upper (lines cross within the range - two regions of significance (although one is outside data range))
# region caped to the data range
arrows(WJN[3,4],0,mx,0, ang=90, length=0.05, code=3)
text(mean(c(WJN[3,4],mx)), 0.003,rownames(WJN)[3])
# since lower>upper (lines cross outside data range
# region caped to the data range if necessary
arrows(min(WJN[2,3],mx),0.01,max(WJN[2,4],mn),0.01, ang=90, length=0.05, code=3)
text(mean(c(min(WJN[2,3],mx), max(WJN[2,4],mn))), 0.013,rownames(WJN)[2])
# since lower>upper (lines cross outside data range
# region caped to the data range if necessary
arrows(min(WJN[1,3],mx),0.02,max(WJN[1,4],mn),0.02, ang=90, length=0.05, code=3)
text(mean(c(min(WJN[1,3],mx),max(WJN[1,4],mn))), 0.023,rownames(WJN)[1])
