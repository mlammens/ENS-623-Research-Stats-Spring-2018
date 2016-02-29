###################################################
### chunk number 1: CorrelationRegression_SetDir
###################################################
setwd("CorrelationRegression/Data")
#setCacheDir("CorrelationRegression/cache")
setCacheDir("cache")


###################################################
### chunk number 2: CovCor
###################################################
getOption("SweaveHooks")[["fig"]]()
source("../../Figure_helper.R")
library(mvtnorm)
library(MASS)
old <- par(mfrow=c(2,3),mar=c(4,4,0.1,0.1),xpd=NA)
#Illustration of covariance vs correlation
seed<-2
set.seed(seed)
for (i in 1:1000) {
data<-mvrnorm(70,c(5,5), Sigma=matrix(c(5,5*.7,5*.7,5),2,2), tol=1, empirical=T)
if (all(data>0) && all(data<10)) break
}
colnames(data)<-c("x","y")
data<-data.frame(data)
minx<-0 #min(data$x)
maxx<-max(pretty(data$x)) #max(data$x)
miny<-0 #min(data$y)
maxy<-max(pretty(data$y)) #max(data$y)
plot(data, axes=F, xlab="", ylab="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="black")
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
#axis(1, pos=0, tick=T, tcl=0,lab=F, at=c(-1,10))
mtext("X units", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(data$y)[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y units",2, line=2)
#cor(data)
#cov(data)
text(upperLeft(), "a)", font=2, cex=1.5)
covText<-substitute(paste("cov = ", cc), list(cc=round(cov(data$x,data$y),3)))
text(10,2,covText,pos=2, cex=1.5)
covText<-substitute(paste("cor = ", cc), list(cc=round(cor(data$x,data$y),3)))
text(10,1,covText, pos=2,cex=1.5)
data1<-data

data$x<-data$x*10
minx<-0 #min(data$x)
maxx<-max(pretty(data$x)) #max(data$x)
miny<-0 #min(data$y)
maxy<-max(pretty(data$y)) #max(data$y)
plot(data, axes=F, xlab="", ylab="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="black")
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
#axis(1, pos=0, tick=T, tcl=0,lab=F, at=c(-1,10))
mtext("X units", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(data$y)[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y units",2, line=2)
#cor(data)
#cov(data)
text(upperLeft(), "b)", font=2, cex=1.5)
covText<-substitute(paste("cov = ", cc), list(cc=round(cov(data$x,data$y),3)))
text(100,2,covText,pos=2, cex=1.5)
covText<-substitute(paste("cor = ", cc), list(cc=round(cor(data$x,data$y),3)))
text(100,1,covText, pos=2,cex=1.5)

data<-data1
data$x <- 10-data1$x
minx<-0 #min(data$x)
maxx<-max(pretty(data$x)) #max(data$x)
miny<-0 #min(data$y)
maxy<-max(pretty(data$y)) #max(data$y)
plot(data, axes=F, xlab="", ylab="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="black")
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
#axis(1, pos=0, tick=T, tcl=0,lab=F, at=c(-1,10))
mtext("X units", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(c(data$y))[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y units",2, line=2)
#cor(data)
#cov(data)
text(upperLeft(), "c)", font=2, cex=1.5)
covText<-substitute(paste("cov = ", cc), list(cc=round(cov(data$x,data$y),3)))
text(10,9,covText,pos=2, cex=1.5)
covText<-substitute(paste("cor = ", cc), list(cc=round(cor(data$x,data$y),3)))
text(10,8,covText, pos=2,cex=1.5)

set.seed(seed)
for (i in 1:1000) {
data<-mvrnorm(70,c(5,5), Sigma=matrix(c(5,5*.95,5*.95,5),2,2), tol=1, empirical=T)
if (all(data>0) && all(data<10)) break
}
colnames(data)<-c("x","y")
data<-data.frame(data)
minx<-0 #min(data$x)
maxx<-max(pretty(data$x)) #max(data$x)
miny<-0 #min(data$y)
maxy<-max(pretty(data$y)) #max(data$y)
plot(data, axes=F, xlab="", ylab="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="black")
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
#axis(1, pos=0, tick=T, tcl=0,lab=F, at=c(-1,10))
mtext("X units", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(c(data$y))[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y units",2, line=2)
#cor(data)
#cov(data)
text(upperLeft(), "d)", font=2, cex=1.5)
covText<-substitute(paste("cov = ", cc), list(cc=round(cov(data$x,data$y),3)))
text(10,2,covText,pos=2, cex=1.5)
covText<-substitute(paste("cor = ", cc), list(cc=round(cor(data$x,data$y),3)))
text(10,1,covText, pos=2,cex=1.5)
set.seed(seed)
for (i in 1:1000) {
data<-mvrnorm(70,c(5,5), Sigma=matrix(c(5,5*.0,5*.0,5),2,2), tol=1, empirical=T)
if (all(data>0) && all(data<10)) break
}
colnames(data)<-c("x","y")
data<-data.frame(data)
minx<-0 #min(data$x)
maxx<-max(pretty(data$x)) #max(data$x)
miny<-0 #min(data$y)
maxy<-max(pretty(data$y)) #max(data$y)
plot(data, axes=F, xlab="", ylab="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="black")
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
#axis(1, pos=0, tick=T, tcl=0,lab=F, at=c(-1,10))
mtext("X units", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(data$y)[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y units",2, line=2)
#cor(data)
#cov(data)
text(upperLeft(), "e)", font=2, cex=1.5)
covText<-substitute(paste("cov = ", cc), list(cc=round(cov(data$x,data$y),3)))
text(10,2,covText,pos=2, cex=1.5)
covText<-substitute(paste("cor = ", cc), list(cc=round(cor(data$x,data$y),3)))
text(10,1,covText, pos=2,cex=1.5)

data<-data1
set.seed(seed)
for (i in 1:1000) {
data<-mvrnorm(23,c(5,5), Sigma=matrix(c(5,5*.7,5*.7,5),2,2), tol=1, empirical=T)
if (all(data>0) && all(data<10)) break
}
colnames(data)<-c("x","y")
data<-data.frame(data)
minx<-0 #min(data$x)
maxx<-max(pretty(data$x)) #max(data$x)
miny<-0 #min(data$y)
maxy<-10 #max(pretty(data$y)) #max(data$y)
plot(data, axes=F, xlab="", ylab="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="gray")
axis(1, pos=miny,tick=T, tcl=0,lab=F, at=pretty(data$x))
#axis(1, pos=0, tick=T, tcl=0,lab=F, at=c(-1,10))
mtext("X", 1)
axis(2,pos=minx,las=1,tcl=0,tick=T, lab=F, at=pretty(data$y))
mtext("Y",2, las=1)
meanx <- mean(data$x)
meany <- mean(data$y)
#range(data$x)
for (i in 1:70){
pp<-i
#print(data$x[pp])
if (data$x[pp]< 3.8 && data$y[pp]< 3.8) break
}
segments(meanx, miny, meanx,maxy, col="black", lty=2)
segments(minx, meany, maxx,meany, col="black",lty=2)
points(data$x[pp], data$y[pp], pch=16)
arrows(data$x[pp]+.1,data$y[pp],meanx,data$y[pp],col="black", length=0.05, code=3,lwd=1.2,lty=1)
arrows(data$x[pp],data$y[pp]+.1,data$x[pp],meany,col="black", length=0.05, code=3,lwd=1.2, lty=1)
#rect(3,2,8.5,1, col="white")
#text(mean(c(data$x[pp],meanx)),data$y[pp],expression(paste("-ve x deviation")),pos=1, cex=1, col="black")
#text(data$x[pp],mean(c(data$y[pp],meany)),srt=90,expression(paste("-ve y deviation")),pos=3, cex=1, col="black")
#text(mean(c(data$x[pp],meanx)),data$y[pp],expression(paste("-ve")),pos=1, cex=1, col="black")
text(mean(c(data$x[pp],meanx)),data$y[pp],expression(paste(x[i]-bar(x), "  (-ve)")),pos=1, cex=1, col="black")
#text(data$x[pp],mean(c(data$y[pp],meany)),srt=90,expression(paste("-ve")),pos=NULL, adj=c(.5,-1),cex=1, col="black")
text(data$x[pp],mean(c(data$y[pp],meany)),srt=90,expression(paste(y[i]-bar(y), "  (-ve)")),pos=NULL, adj=c(.5,-.5),cex=1, col="black")
#text(5.2,3,expression(paste(Dev==sum((x-bar(x))(y-bar(y))))),pos=4,cex=1, col="black")
text(mean(c(minx,meanx)),maxy,expression(paste("-ve deviation")),pos=NULL,adj=c(.5,4), cex=1.5, col="black")
text(mean(c(meanx,maxx)),miny,expression(paste("-ve deviation")),pos=3, cex=1.5, col="black")
text(mean(c(minx,meanx)),miny,expression(paste("+ve deviation")),pos=3, cex=1.5, col="black")
text(mean(c(meanx,maxx)),maxy,expression(paste("+ve deviation")),pos=NULL, adj=c(.5,4),cex=1.5, col="black")
text(upperLeft(), "f)", font=2, cex=1.5)
par(old)


###################################################
### chunk number 3: regressionmodel
###################################################
getOption("SweaveHooks")[["fig"]]()
#postscript(file="regressionmodel.ps", width=9, height=3,paper='special', horizontal=F)
old <- par(mfrow=c(1,3),mar=c(4,4,0.1,0.1),xpd=NA)

plot(c(0,1),c(0,1), axes=F, xlab="", ylab="", pch=16, type="n")
axis(1,pos=0, tick=T, tcl=0,lab=F)
mtext("Independent (X)", 1)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=seq(1,4,l=4), at=seq(.25,1,l=4))
lines(c(0,0),c(1,0))
mtext("Dependent (Y)", 2, line=2)
text(upperLeft(), "a)", font=2, cex=1.5)
text(.9,.5,srt=0,expression(paste(Slope (beta[1]) == "0")),cex=1.5,adj=c(1,0))
lines(c(0,1),c(.5,.5),lty=2)
#abline(.5,0,xpd=F, lty=2)
text(.9,.9,srt=45/2,expression(paste(beta[1],">0")),cex=1.5,adj=c(1,1))
lines(c(0,1),c(.5,1),lty=2)
text(.9,.1,srt=-45/2,expression(paste(beta[1],"<0")),cex=1.5,adj=c(1,-0))
lines(c(0,1),c(.5,0),lty=2)
 text(0,0,srt=0,expression(paste("Y-intercept", (beta[0]) == "2 for all")),cex=1.5,adj=c(0,0))

plot(c(0,1),c(0,1), axes=F, xlab="", ylab="", pch=16, type="n")
axis(1,pos=0, tick=T, tcl=0,lab=F)
mtext("Independent (X)", 1)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=seq(1,4,l=4), at=seq(.25,1,l=4))
lines(c(0,0),c(1,0))
mtext("Dependent (Y)", 2, line=2)
text(upperLeft(), "b)", font=2, cex=1.5)
text(.9,.9,srt=45/2,expression(paste(beta[0],"=2, ",beta[1],">0")),cex=1.5,adj=c(1,1))
lines(c(0,1),c(.25,1-.25),lty=2)
text(.9,.9-.25,srt=45/2,expression(paste(beta[0],"=1, ",beta[1],">0")),cex=1.5,adj=c(1,1))
lines(c(0,1),c(.5,1),lty=2)
text(.9,.9-.5,srt=45/2,expression(paste(beta[0],"=0, ",beta[1],">0")),cex=1.5,adj=c(1,1))
lines(c(0,1),c(0,1-.5),lty=2)
 
plot(c(0,1),c(0,1), axes=F, xlab="", ylab="", pch=16, type="n")
axis(1,pos=0, tick=T, tcl=0,lab=F)
mtext("Independent (X)", 1)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=c(seq(0,3,l=4)), at=seq(.25,1,l=4))
lines(c(0,0),c(1,0))
mtext("Dependent (Y)", 2, line=2)
text(upperLeft(), "c)", font=2, cex=1.5)
text(.9,1-.25,srt=0,expression(paste(beta[0],"=2, ",beta[1],"=0")),cex=1.5,adj=c(1,0))
lines(c(0,1),c(.25,.25),lty=2)
text(.9,1-.5,srt=0,expression(paste(beta[0],"=1, ",beta[1],"=0")),cex=1.5,adj=c(1,0))
lines(c(0,1),c(.5,.5),lty=2)
text(.9,1-.75,srt=0,expression(paste(beta[0],"=0, ",beta[1],"=0")),cex=1.5,adj=c(1,0))
lines(c(0,1),c(.75,.75),lty=2)

par(old)


###################################################
### chunk number 4: Fields
###################################################
require(fields, quiet=TRUE)


###################################################
### chunk number 5: regressionpartitioning
###################################################
getOption("SweaveHooks")[["fig"]]()
seed<-20
#Figure that illustrates partitioning of SS in regression
#postscript(file="regressionpartitioning.ps", width=9, height=9,paper='special', horizontal=F)
old <- par(mfrow=c(2,2),mar=c(4,4,0.1,0.1),xpd=F)
set.seed(163)
data<-mvrnorm(10,c(6,7), Sigma=matrix(c(20,20*.9,20*.9,20),2,2), tol=1, empirical=T)
colnames(data)<-c("x","y")
data<-data.frame(data)
minx<-0 #min(data$x)
maxx<-max(pretty(data$x)) #max(data$x)
miny<-0 #min(data$y)
maxy<-max(pretty(data$y)) #max(data$y)
plot(data, axes=F, xlab="", ylab="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="black")
lines(c(0,12),c(mean(data$y),mean(data$y)))
#abline(lm(data$y~data$x))
segments(data$x,data$y,data$x,mean(data$y))
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
#axis(1, pos=0, tick=T, tcl=0,lab=F, at=c(-1,10))
mtext("X units", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(data$y)[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y units",2, line=2)
#cor(data)
#cov(data)
text(upperLeft(), "a)", font=2, cex=1.5)
text(8,4,expression(paste(SS[total],"=")), adj=c(1,0))
text(8,4,expression(paste("sum of squared")), adj=c(0,0))
text(8,3.5,expression(paste("total distances")), adj=c(0,0))
text(8,2.5,expression(paste(MS[total],"=")), adj=c(1,0))
text(8,2.5,expression(paste("response variance")), adj=c(0,0))
text(8,1.5,expression(paste("=")), adj=c(1,0))
text(8,1.5,expression(paste(frac(SS[total],df[total]))), adj=c(0,.5))
text(4,10,"Overall mean", pos=2)
arrows(4,10,5,mean(data$y)+.1,code=2, length=.1)

#seed<-20
#Figure that illustrates partitioning of SS in regression
#postscript(file="regressionpartitioning.ps", width=9, height=9,paper='special', horizontal=F)
#old <- par(mfrow=c(2,2),mar=c(4,4,0.1,0.1),xpd=F)
#set.seed(163)
#data<-mvrnorm(10,c(6,7), Sigma=matrix(c(20,20*.9,20*.9,20),2,2), tol=1, empirical=T)
#colnames(data)<-c("x","y")
#data<-data.frame(data)
minx<-0 #min(data$x)
maxx<-max(pretty(data$x)) #max(data$x)
miny<-0 #min(data$y)
maxy<-max(pretty(data$y)) #max(data$y)
plot(data, axes=F, xlab="", ylab="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="black")
lines(c(0,12),c(mean(data$y),mean(data$y)))
abline(ll<-lm(data$y~data$x))
pred<-predict(lm(data$y~data$x))
segments(data$x,pred,data$x,mean(data$y))
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
#axis(1, pos=0, tick=T, tcl=0,lab=F, at=c(-1,10))
mtext("X units", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(data$y)[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y units",2, line=2)
#cor(data)
#cov(data)
text(upperLeft(), "b)", font=2, cex=1.5)
text(8,4.5,expression(paste(SS[regression],"=")), adj=c(1,.5))
text(8,4.5,expression(paste("sum of squared")), adj=c(0,.5))
text(8,4,expression(paste("explained distances")), adj=c(0,.5))
text(8,3,expression(paste(MS[regression],"=")), adj=c(1,.5))
text(8,3,expression(paste("conservative mean")), adj=c(0,.5))
text(8,2.5,expression(paste("var explained")), adj=c(0,.5))
text(8,1,expression(paste("=")), adj=c(1,.5))
text(8,1,expression(paste(frac(SS[regression],df[regression]))), adj=c(0,.5))
text(4,10,"Overall mean", pos=2)
ll<-10
arrow.plot(4,10, ll,-ll*3, length=.1,true.angle=T,arrow.ex=.22)
text(6,12,"Predicted trend", pos=2)
ll<-10
arrow.plot(6,12, ll,-ll*3, length=.1,true.angle=T,arrow.ex=.3)
text(10,14,"Explained variabilty\n(distances)", pos=2)
ll<-10
arrow.plot(10,14, ll,-ll*3, length=.1,true.angle=T,arrow.ex=.27)

plot(data, axes=F, xlab="", ylab="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="black")
#lines(c(0,12),c(mean(data$y),mean(data$y)))
abline(lm(data$y~data$x))
pred<-predict(lm(data$y~data$x))
segments(data$x,pred,data$x,data$y)
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
#axis(1, pos=0, tick=T, tcl=0,lab=F, at=c(-1,10))
mtext("X units", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(data$y)[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y units",2, line=2)
#cor(data)
#cov(data)
text(upperLeft(), "c)", font=2, cex=1.5)
text(7.8,4.5,expression(paste(SS[residual],"=")), adj=c(1,.5))
text(7.8,4.5,expression(paste("sum of squared")), adj=c(0,.5))
text(7.8,4,expression(paste("unexplained distances")), adj=c(0,.5))
text(7.8,3,expression(paste(MS[residual],"=")), adj=c(1,.5))
text(7.8,3,expression(paste("conservative mean")), adj=c(0,.5))
text(7.8,2.5,expression(paste("var unexplained")), adj=c(0,.5))
text(7.8,1,expression(paste("=")), adj=c(1,.5))
text(7.8,1,expression(paste(frac(SS[residual],df[residual]))), adj=c(0,.5))
text(6,12,"Predicted trend", pos=2)
ll<-10
arrow.plot(6,12, ll,-ll*3, length=.1,true.angle=T,arrow.ex=.3)
text(10,14,"Explained variabilty\n(distances)", pos=2)
ll<-10
arrow.plot(10,14, ll,-ll*0, length=.1,true.angle=T,arrow.ex=.1)

plot(seq(0,5,length=100), df(seq(0,5,length=100), df1=3, df2=10),type='l',xlab='', ylab='',axes=F,ylim=c(0,1))
axis(1,pos=0)
#axis(2,pos=0,las=1,tick=T,tcl=0,lab=F)
#lines(c(0,5),c(0,0), col='gray')
#box()
rect(1,.85,5.2,1.04,col='white')
text(1.25,.92,expression(paste("F-ratio=",over(Explained,Unexplained), " = ",over(MS[groups], MS[residual]))),pos=4)
text(2,.5,expression(paste('F-distribution\n(Distribution of all possible\nexpected F-ratios when the')),pos=4)
text(2,.44,expression(paste(H[0],' is true)')),pos=4)
#text(3,.5,'Distribution of all possible\nexpected F-ratios when the H0 is true',pos=4)
arrows(2,.5,1.5,.4,code=2,length=.1)
text(upperLeft(), "d)", font=2, cex=1.5)

par(old)


###################################################
### chunk number 6: regressionAssumptions1
###################################################
getOption("SweaveHooks")[["fig"]]()
#normally distributed
set.seed(1)
old_par<- par(mar=c(1.5,1.5,0,0))
x <- seq(1,10,by=1)
y <- replicate(1,rnorm(10,x,.5))
yy <- x
plot(x,y, asp=NULL,axes=F, xlab="", ylab="", lwd=2, xlim=c(0,12),type="n",ylim=c(0,12),pch=16, cex=1.5)
axis(1, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2, at=seq(0,12,b=1))
mtext("Predictor",1, font=2,cex=1.5)
axis(2, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2,at=seq(0,12,b=1))
mtext("Response",2, font=2,cex=1.5)
#axis(1, tick=T,tcl=0,lab=F,pos=-2,cex.axis=1, lwd=2, at=seq(0,12,b=1))
#mtext("Predictor",1, font=2,cex=1.5)
#axis(2, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2,at=seq(-2,12,b=1))
#mtext("Response",2, font=2,cex=1.5)
l <- lm(yy~x)
abline(l, col="gray",lty=1, lwd=2)
y1 <- replicate(1,rnorm(10000,1,.5))
y2 <- replicate(1,rnorm(10000,2,.5))
y3 <- replicate(1,rnorm(10000,3,.5))
y4 <- replicate(1,rnorm(10000,4,.5))
y5 <- replicate(1,rnorm(10000,5,.5))
y6 <- replicate(1,rnorm(10000,6,.5))
y7 <- replicate(1,rnorm(10000,7,.5))
y8 <- replicate(1,rnorm(10000,8,.5))
y9 <- replicate(1,rnorm(10000,9,.5))
y10 <- replicate(1,rnorm(10000,10,.5))
yyy<-cbind(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10)
yy <- apply(yyy,2,mean)
boxplot(yyy, outline=F,add=T, axes=F, border="gray")
points(x,y,col="black", pch=16, cex=1.5)
boxplot(yy, outline=F,add=T, at=12,axes=F, border="black", width=2)
text(upperLeft(), "a)", font=2, cex=1.5)
par(old_par)


###################################################
### chunk number 7: regressionAssumptions2
###################################################
getOption("SweaveHooks")[["fig"]]()
#lognormal figure
set.seed(3)
old_par<- par(mar=c(1.5,1.5,0,0))
x <- seq(1,10,by=1)
y <- replicate(1,rlnorm(10,log(x),.5^2))
yy <- x
plot(x,y, asp=NULL,axes=F, xlab="", ylab="", lwd=2, xlim=c(0,12),type="n",ylim=c(0,16),pch=16, cex=1.5)
axis(1, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2, at=seq(0,12,b=1))
mtext("Predictor",1, font=2,cex=1.5)
axis(2, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2,at=seq(0,16,b=1))
mtext("Response",2, font=2,cex=1.5)
l <- lm(yy~x)
abline(l, col="gray",lty=1, lwd=2)
y1 <- replicate(1,rlnorm(10000,log(1),.95^2))
y2 <- replicate(1,rlnorm(10000,log(1),.95^2))+1
y3 <- replicate(1,rlnorm(10000,log(1),.95^2))+2
y4 <- replicate(1,rlnorm(10000,log(1),.95^2))+3
y5 <- replicate(1,rlnorm(10000,log(1),.95^2))+4
y6 <- replicate(1,rlnorm(10000,log(1),.95^2))+5
y7 <- replicate(1,rlnorm(10000,log(1),.95^2))+6
y8 <- replicate(1,rlnorm(10000,log(1),.95^2))+7
y9 <- replicate(1,rlnorm(10000,log(1),.95^2))+8
y10 <- replicate(1,rlnorm(10000,log(1),.95^2))+9
yyy<-cbind(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10)
boxplot(yyy, outline=F,add=T, axes=F, border="gray")
#points(rep(x,times=10),replicate(10,rnorm(10,x,1)), col="gray",pch=16)
#boxplot(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10, outline=F,add=T, axes=F, border="black")
points(x,y,col="black", pch=16, cex=1.5)
boxplot(rlnorm(1000,log(x),.5^2), outline=F,add=T, at=12,axes=F, border="black", width=2)
text(upperLeft(), "b)", font=2, cex=1.5)
par(old_par)


###################################################
### chunk number 8: regressionAssumptions3
###################################################
getOption("SweaveHooks")[["fig"]]()
#normally distributed
set.seed(6)
old_par<- par(mar=c(1.5,1.5,0,0))
x <- seq(1,10,by=1)
y <- replicate(1,rnorm(10,x,2))
yy <- x
plot(x,y, asp=NULL,axes=F, xlab="", ylab="", lwd=2, xlim=c(0,12),type="n",ylim=c(0,12),pch=16, cex=1.5)
axis(1, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2, at=seq(0,12,b=1))
mtext("Predictor",1, font=2,cex=1.5)
axis(2, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2,at=seq(0,12,b=1))
mtext("Response",2, font=2,cex=1.5)
#axis(1, tick=T,tcl=0,lab=F,pos=-2,cex.axis=1, lwd=2, at=seq(0,12,b=1))
#mtext("Predictor",1, font=2,cex=1.5)
#axis(2, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2,at=seq(-2,12,b=1))
#mtext("Response",2, font=2,cex=1.5)
l <- lm(yy~x)
abline(l, col="gray",lty=1, lwd=2)
y1 <- replicate(1,rnorm(10000,1,1))
y2 <- replicate(1,rnorm(10000,2,1))
y3 <- replicate(1,rnorm(10000,3,1))
y4 <- replicate(1,rnorm(10000,4,1))
y5 <- replicate(1,rnorm(10000,5,1))
y6 <- replicate(1,rnorm(10000,6,1))
y7 <- replicate(1,rnorm(10000,7,1))
y8 <- replicate(1,rnorm(10000,8,1))
y9 <- replicate(1,rnorm(10000,9,1))
y10 <- replicate(1,rnorm(10000,10,1))
yyy<-cbind(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10)
yy <- apply(yyy,2,mean)
boxplot(yyy, outline=F,add=T, axes=F, border="gray")
points(x,y,col="black", pch=16, cex=1.5)
l <- lm(y~x)
boxplot(yy, outline=F,add=T, at=12,axes=F, border="black", width=2)
text(upperLeft(), "a)", font=2, cex=1.5)
par(old_par)


###################################################
### chunk number 9: regressionAssumptions4
###################################################
getOption("SweaveHooks")[["fig"]]()
py<-(l$coef[2]*x)+l$coef[1]
old_par<- par(mar=c(1.5,1.5,0,0))
resid <- l$resid
#y <- replicate(1,rnorm(10,x,2))
#yy <- x
plot(py,resid, axes=F, xlab="", ylab="", lwd=2, xlim=c(0,11),type="n",ylim=c(-4,4),pch=16, cex=1.5)
axis(1, tick=T,tcl=0,lab=F,pos=-4,cex.axis=1, lwd=2, at=seq(0,11,b=1))
mtext("Predicted (expected) value",1, font=2,cex=1.5)
axis(2, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2,at=seq(-4,4,b=1))
mtext("Residual",2, font=2,cex=1.5)
points(py,resid,col="black", pch=16, cex=1.5)
#boxplot(y, outline=F,add=T, at=12,axes=F, border="black")
lines(c(0,11),c(0,0), lty=2)
text(upperLeft(), "b)", font=2, cex=1.5)
par(old_par)


###################################################
### chunk number 10: regressionAssumptions5
###################################################
getOption("SweaveHooks")[["fig"]]()
#lognormal figure
set.seed(10)
old_par<- par(mar=c(1.5,1.5,0,0))
x <- seq(1,10,by=1)
ress <- seq(0,2,l=10)
ress <- jitter(ress, ress*10)*2
y<-1:10+(ress*c(1,-1))
l<-lm(y~x)

#y <- replicate(1,rlnorm(10,log(x),.5^2))
yy <- x
plot(x,y, asp=NULL,axes=F, xlab="", ylab="", lwd=2, xlim=c(0,12),type="n",ylim=c(0,16),pch=16, cex=1.5)
axis(1, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2, at=seq(0,12,b=1))
mtext("Predictor",1, font=2,cex=1.5)
axis(2, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2,at=seq(0,16,b=1))
mtext("Response",2, font=2,cex=1.5)
abline(l, col="gray",lty=1, lwd=2)
y1 <- replicate(1,rlnorm(10000,log(1),.5^2))+1
y2 <- replicate(1,rlnorm(10000,log(1),.61^2))+1.33
y3 <- replicate(1,rlnorm(10000,log(1),.73^2))+1.67
y4 <- replicate(1,rlnorm(10000,log(1),.83^2))+2
y5 <- replicate(1,rlnorm(10000,log(1),0.94^2))+2.33
y6 <- replicate(1,rlnorm(10000,log(1),1.05^2))+2.67
y7 <- replicate(1,rlnorm(10000,log(1),1.17^2))+3
y8 <- replicate(1,rlnorm(10000,log(1),1.28^2))+3.33
y9 <- replicate(1,rlnorm(10000,log(1),1.39^2))+3.67
y10 <- replicate(1,rlnorm(10000,log(1),1.5^2))+4
yyy<-cbind(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10)
boxplot(yyy, outline=F,add=T, axes=F, border="gray")
#points(rep(x,times=10),replicate(10,rnorm(10,x,1)), col="gray",pch=16)
#boxplot(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10, outline=F,add=T, axes=F, border="black")
points(x,y,col="black", pch=16, cex=1.5)
l <- lm(y~x)
boxplot(rlnorm(10000,log(1),1.5^2)+1, outline=F,add=T, at=12,axes=F, border="black", width=2)
#boxplot(rlnorm(1000,log(x),.5^2), outline=F,add=T, at=12,axes=F, border="black", width=2)
text(upperLeft(), "c)", font=2, cex=1.5)
par(old_par)


###################################################
### chunk number 11: regressionAssumptions6
###################################################
getOption("SweaveHooks")[["fig"]]()
py<-(l$coef[2]*x)+l$coef[1]
old_par<- par(mar=c(1.5,1.5,0,0))
resid <- l$resid
#y <- replicate(1,rnorm(10,x,2))
#yy <- x
plot(py,resid, axes=F, xlab="", ylab="", lwd=2, xlim=c(0,11),type="n",ylim=c(-4,4),pch=16, cex=1.5)
axis(1, tick=T,tcl=0,lab=F,pos=-4,cex.axis=1, lwd=2, at=seq(0,11,b=1))
mtext("Predicted (expected) value",1, font=2,cex=1.5)
axis(2, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2,at=seq(-4,4,b=1))
mtext("Residual",2, font=2,cex=1.5)
points(py,resid,col="black", pch=16, cex=1.5)
#boxplot(y, outline=F,add=T, at=12,axes=F, border="black")
lines(c(0,11),c(0,0), lty=2)
text(upperLeft(), "d)", font=2, cex=1.5)
par(old_par)


###################################################
### chunk number 12: modelIvsIIregression
###################################################
getOption("SweaveHooks")[["fig"]]()
seed<-20
#postscript(file="modelIvsIIregression.ps", width=9, height=6,paper='special', horizontal=F)
#old <- par(mfrow=c(2,2),mar=c(4,4,0.1,0.1),xpd=F)
old <- par(mfrow=c(2,3),mar=c(4,4,0.1,0.1),xpd=F)

seed<-2
set.seed(seed)
for (i in 1:1000) {
data<-mvrnorm(35,c(5,6), Sigma=matrix(c(5,5*.7,5*.7,5),2,2), tol=1, empirical=T)
if (all(data>0) && all(data<10)) break
}
colnames(data)<-c("x","y")
data<-data.frame(data)
data$x<-data$x*10
#Plot1-OLS Y vs X
minx<-0 #min(data$x)
maxx<-max(pretty(data$x)) #max(data$x)
miny<-0 #min(data$y)
maxy<-max(pretty(data$y)) #max(data$y)
plot(data, axes=F, xlab="", ylab="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="black")
abline(lm(y~x,data), lwd=2)
l<- lm(y~x,data)
b<-l$coef[2]
a<-l$coef[1]
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
#axis(1, pos=0, tick=T, tcl=0,lab=F, at=c(-1,10))
mtext("X units", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(data$y)[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y units",2, line=2)
l<-lm(y~x,data)
b<-l$coef[2]
a<-l$coef[1]
segments(data$x,fitted(lm(y~x,data)),data$x, data$y, col="black", lwd=1)
points(y~x,data, pch=16)
text(upperLeft(.1), "a) OLS Y against X", font=2, cex=1.5,adj=c(0,0))
data1<-data

plot(data, axes=F, xlab="", ylab="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="black")
#abline(lm(x~y,data), lwd=2)
l<- lm(x~y,data)
b<-l$coef[2]
b<-1/b
a<-mean(data$y)-(b*mean(data$x))
abline(a,b, col="black",lwd=2)
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
#axis(1, pos=0, tick=T, tcl=0,lab=F, at=c(-1,10))
mtext("X units", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(data$y)[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y units",2, line=2)
segments(data$x,b*data$x+a,data$x, data$y, col="black", lwd=1)
points(y~x,data, pch=16)
text(upperLeft(.1), "b) OLS X against Y", font=2, cex=1.5,adj=c(0,0))

#MA regression plot
data$x<-data$x/10
#data<-data2
minx<-0 #min(data$x)
maxx<-max(pretty(data$x)) #max(data$x)
miny<-0 #min(data$y)
maxy<-max(pretty(data$y)) #max(data$y)
plot(data, axes=F, xlab="", ylab="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="black")
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
#axis(1, pos=0, tick=T, tcl=0,lab=F, at=c(-1,10))
mtext("X units", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(data$y)[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y units",2, line=2)
r<-princomp(cbind(data$x,data$y),cor=F)
u<-r$loadings
p<-matrix(c(1,0,0,0), nrow=2)
X<-rbind(data$x,data$y)
X<-r$center + solve(u,p %*% u %*% (X-r$center))
b3<-r$loadings[2,1]/r$loadings[1,1]
a3<-r$center[2]-b3*r$center[1]
abline(a3,b3,col="black",lwd=1)
segments(data$x,data$y, X[1,],X[2,],lwd=1,col="black")
points(y~x,data, pch=16,las=1)
text(upperLeft(.1), "c) MA", font=2, cex=1.5,adj=c(0,0))

#ranged MA regression plot
data<-data1
minx<-0 #min(data$x)
maxx<-max(pretty(data$x)) #max(data$x)
miny<-0 #min(data$y)
maxy<-max(pretty(data$y)) #max(data$y)
plot(data, axes=F, xlab="", ylab="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="black")
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
#axis(1, pos=0, tick=T, tcl=0,lab=F, at=c(-1,10))
mtext("X units", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(data$y)[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y units",2, line=2)
data$x<-(data$x - min(data$x))/(max(data$x) - min(data$x))
data$y<-(data$y - min(data$y))/(max(data$y) - min(data$y))
data2<-data
r<-princomp(cbind(data$x,data$y),cor=F)
u<-r$loadings
p<-matrix(c(1,0,0,0), nrow=2)
X<-rbind(data$x,data$y)
X<-r$center + solve(u,p %*% u %*% (X-r$center))
b3<-r$loadings[2,1]/r$loadings[1,1]
a3<-r$center[2]-b3*r$center[1]
b3 <- b3 * ((max(data1$y) - min(data1$y))/(max(data1$x) - min(data1$x)))
X[1,]<-(X[1,]*(max(data1$x) - min(data1$x)))+ min(data1$x)
X[2,]<-(X[2,]*(max(data1$y) - min(data1$y)))+ min(data1$y)
a3 <- mean(data1$y) - b3 * mean(data1$x)
rMAa<-a3
rMAb<-b3
#print(m2regress(data1$x,data1$y,type="rMA"))
#b3<-m2regress(data1$x,data1$y,type="rMA")$coef[2]
#a3<-m2regress(data1$x,data1$y,type="rMA")$coef[1]
abline(a3,b3,col="black",lwd=2)
segments(data1$x,data1$y, X[1,],X[2,],lwd=1,col="black")
#points(y~x,data1, pch=16,las=1)
text(upperLeft(.1), "d) ranged MA", font=2, cex=1.5,adj=c(0,0))


#Reduced MA regression plot
data<-data1
minx<-0 #min(data$x)
maxx<-max(pretty(data$x)) #max(data$x)
miny<-0 #min(data$y)
maxy<-max(pretty(data$y)) #max(data$y)
plot(data, axes=F, xlab="", ylab="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="black")
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
#axis(1, pos=0, tick=T, tcl=0,lab=F, at=c(-1,10))
mtext("X units", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(data$y)[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y units",2, line=2)
SSy<-sd(data$y)
SSx<-sd(data$x)
b5<-SSy/SSx
a5<-mean(data$y)-(b5*mean(data$x))
l<-lm(y~x,data)
a6<-l$coef[1]
b6<-l$coef[2]
ll <-lm(x~y,data)
a6<-ll$coef[1]
b6<-ll$coef[2]
f1<-(b5*data$x)+a5
f2<-(data$y-a5)/b5
i<-41
for(i in 1:length(data$y)){
polygon(c(data$x[i],data$x[i],f2[i],data$x[i]),c(data$y[i],f1[i],data$y[i],data$y[i]),col="lightgray", density=50)
}
i<-20
polygon(c(data$x[i],data$x[i],f2[i],data$x[i]),c(data$y[i],f1[i],data$y[i],data$y[i]),col="gray")
segments(data1$x,data$y,data$x,f1, col="black", lwd=1,lty=2)
segments(data1$x,data$y,f2,data$y, col="black", lwd=1, lty=2)
abline(a5,b5,col="black",lwd=2)
points(y~x, data,pch=16)
polygon(c(data$x[i],data$x[i],f2[i],data$x[i]),c(data$y[i],f1[i],data$y[i],data$y[i]),col="gray")
text(upperLeft(.1), "e) RMA", font=2, cex=1.5,adj=c(0,0))

#All regression plots
 data<-data1
minx<-0 #min(data$x)
maxx<-max(pretty(data$x)) #max(data$x)
miny<-0 #min(data$y)
maxy<-max(pretty(data$y)) #max(data$y)
plot(data, axes=F, xlab="", ylab="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="black")
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
#axis(1, pos=0, tick=T, tcl=0,lab=F, at=c(-1,10))
mtext("X units", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(data$y)[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y units",2, line=2)
abline(lm(y~x,data),lwd=1)
text(90,3,"OLS Y vs X\n&MA",pos=2,col="black",cex=1.25)
lines(c(90,96),c(3,3))
arrows(96,3,96,9,length=.05)
l<-lm(x~y,data)
a2<-ll$coef[1]
b2<-ll$coef[2]
b2<-1/b2
a2<-mean(data$y)-(b2*mean(data$x))
abline(a2,b2, col="black",lwd=1)
text(60,.5,"OLS X vs Y",pos=4,col="black",cex=1.25)
arrows(60,.5,13,.5,length=.05)
r<-princomp(cbind(data$x,data$y),cor=F)
u<-r$loadings
p<-matrix(c(1,0,0,0), nrow=2)
X<-rbind(data1$x,data1$y)
X<-r$center + solve(u,p %*% u %*% (X-r$center))
b3<-r$loadings[2,1]/r$loadings[1,1]
a3<-r$center[2]-b3*r$center[1]
abline(a3,b3,col="black",lwd=1)
abline(a5,b5,col="black",lwd=1)
text(10,8,"RMA",pos=3,col="black",cex=1.25)
arrows(10,8,10,2.1,length=.05)
abline(rMAa,rMAb,col="black",lwd=1)
text(60,1.75,"ranged MA",pos=4,col="black",cex=1.25)
arrows(60,1.75,13,2,length=.05)
text(upperLeft(.1), "f)", font=2, cex=1.5,adj=c(0,0))
par(old)


###################################################
### chunk number 13: CorrReg1 eval=FALSE
###################################################
## library(car)
## scatterplot(V1~V2, dataset)


###################################################
### chunk number 14: CorrReg2 eval=FALSE
###################################################
## library(car)
## scatterplot(V1~V2, dataset, reg.line=F)


###################################################
### chunk number 15: CorrReg3 eval=FALSE
###################################################
## corr.test(~V1+V2,data=dataset)


###################################################
### chunk number 16: CorrReg4 eval=FALSE
###################################################
## cor.test(~V1+V2,data=dataset, method="spearman")


###################################################
### chunk number 17: CorrReg5 eval=FALSE
###################################################
## cor.test(~V1+V2,data=dataset, method="kendall")


###################################################
### chunk number 18: CorrReg6 eval=FALSE
###################################################
## library(car)
## scatterplot(DV~IV, dataset)


###################################################
### chunk number 19: CorrReg7 eval=FALSE
###################################################
## library(biology)
## data.lm <- lm.II(DV~IV, christ, type="RMA")
## summary(data.lm)


###################################################
### chunk number 20: CorrReg8 eval=FALSE
###################################################
## dataset.lm <- lm(IV~DV, dataset)
## plot(dataset.lm)
## influence.measures(dataset.lm)
## summary(dataset.lm)


###################################################
### chunk number 21: CorrReg9 eval=FALSE
###################################################
## anova(lm(DV~IV + as.factor(IV), dataset))


###################################################
### chunk number 22: CorrReg10 eval=FALSE
###################################################
## dataset.lm<-lm(DV~IV, dataset)
## summary(dataset.lm)


###################################################
### chunk number 23: CorrReg11 eval=FALSE
###################################################
## dataset.lm<-aov(DV~IV + Error(as.factor(IV)), dataset)
## summary(dataset.lm)
## lm(DV~IV, dataset)


###################################################
### chunk number 24: CorrReg12 eval=FALSE
###################################################
## stat<-function(data,index) {
## summary(lm(DV~IV, data))$coef[2,3]
## }
## rand.gen<- function(data,mle) {
## out<-data
## out$IV <-sample(out$IV, replace=F)
## out
## }
## library(boot)
## dataset.boot<-boot(dataset, stat, R=5000, sim="parametric", ran.gen=rand.gen)
## plot(dataset.boot)
## dataset.boot


###################################################
### chunk number 25: CorrReg13 eval=FALSE
###################################################
## library(MASS)
## data.rlm <- rlm(DV~IV, dataset)


###################################################
### chunk number 26: CorrReg14 eval=FALSE
###################################################
## library(mblm)
## data.mblm <- mblm(DV~IV, dataset, repeated=F)
## summary(data.mblm)


###################################################
### chunk number 27: CorrReg15 eval=FALSE
###################################################
## library(mblm)
## data.mblm <- mblm(DV~IV, dataset, repeated=T)
## summary(data.mblm)


###################################################
### chunk number 28: CorrReg16 eval=FALSE
###################################################
## confint(model, level=0.95)


###################################################
### chunk number 29: CorrReg17 eval=FALSE
###################################################
## par.boot<-function(dataset,index) {
## x<-dataset$ALT[index]
## y<-dataset$HK[index]
## model<-lm(y~x)
## coef(model)
## }
## dataset.boot<-boot(dataset, par.boot,R=5000)
## boot.ci(dataset.boot, index=2)


###################################################
### chunk number 30: CorrReg17 eval=FALSE
###################################################
## predict(model, data.frame(IV=c()), interval="p")


###################################################
### chunk number 31: CorrReg18 eval=FALSE
###################################################
## pred.boot<-function(dataset,index) {
## dataset.rs<-dataset[index,]
## dataset.lm<-lm(HK~ALT, dataset.rs)
## predict(dataset.lm, data.frame(ALT=1))
## }
## dataset.boot<-boot(dataset, pred.boot,R=5000)
## boot.ci(dataset.boot)


###################################################
### chunk number 32: CorrReg19 eval=FALSE
###################################################
## plot(V1~V2, data, pch=16, axes=F, xlab="", ylab="")
## axis(1, cex.axis=.8)
## mtext(text="x-axis title", side=1, line=3)
## axis(2, las=1)
## mtext(text="y-axis title", side=2, line=3)
## box(bty="l")


###################################################
### chunk number 33: CorrReg20 eval=FALSE
###################################################
## data.ellipse(V2, V1, levels=.95, add=T)


###################################################
### chunk number 34: CorrReg21 eval=FALSE
###################################################
## abline(model)


###################################################
### chunk number 35: CorrReg22 eval=FALSE
###################################################
## x <- seq(min(IV), max(IV),l=1000)
## y<-predict(object, data.frame(IV=x), interval="c")
## matlines(x,y, lty=1, col=1)


###################################################
### chunk number 36: Options
###################################################
op <- options(width=75)


###################################################
### chunk number 37: Crabs1
###################################################
crabs <- read.table('crabs.csv', header=T, sep=',')


###################################################
### chunk number 38: CrabsScatterplot
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
scatterplot(GILLWT~BODYWT, data=crabs, reg.line=F)


###################################################
### chunk number 39: CrabsCorrTest
###################################################
cor.test(~GILLWT + BODYWT, data=crabs)


###################################################
### chunk number 40: GreenReadTable
###################################################
green <- read.table('green.csv', header=T, sep=',')


###################################################
### chunk number 41: TEMP
###################################################
op <- options(width=40)


###################################################
### chunk number 42: GreenScatterplotLS
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
scatterplot(BURROWS~TOTMASS, data=green, 
subset=SITE=='LS', reg.line=F)


###################################################
### chunk number 43: GreenScatterplotDS
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
scatterplot(BURROWS~TOTMASS, data=green, 
subset=SITE=='DS', reg.line=F)


###################################################
### chunk number 44: TEMP1
###################################################
options(op)


###################################################
### chunk number 45: GreenLSCorr
###################################################
cor.test(~BURROWS + TOTMASS, data=green, 
subset=SITE=='LS', method='spearman')


###################################################
### chunk number 46: GreenDSCorr
###################################################
cor.test(~BURROWS + TOTMASS, data=green, 
subset=SITE=='DS', method='spearman')


###################################################
### chunk number 47: Temp
###################################################
op <- options(width=30)


###################################################
### chunk number 48: greenPlotLS
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(BURROWS~TOTMASS, data=green, subset=SITE=='LS', xlim=c(0,8), ylim=c(0,80))
with(subset(green, SITE=='LS'), data.ellipse(TOTMASS, BURROWS, levels=.95, add=T))


###################################################
### chunk number 49: greenPlotDS
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(BURROWS~TOTMASS, data=green, 
subset=SITE=='DS', xlim=c(0,8), 
ylim=c(0,150))
with(subset(green, SITE=='DS'), 
data.ellipse(TOTMASS, BURROWS, 
levels=.95, add=T))


###################################################
### chunk number 50: Temp1
###################################################
options(op)


###################################################
### chunk number 51: NelsonReadTable
###################################################
nelson <- read.table('nelson.csv', header=T, sep=',')


###################################################
### chunk number 52: nelsonScatterplot
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
scatterplot(WEIGHTLOSS~HUMIDITY, data=nelson)


###################################################
### chunk number 53: NelsonFigHooks
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(2,2))))


###################################################
### chunk number 54: nelsonResiduals
###################################################
getOption("SweaveHooks")[["fig"]]()
nelson.lm <- lm(WEIGHTLOSS~HUMIDITY, nelson)
plot(nelson.lm)


###################################################
### chunk number 55: NelsonInfluenceMeasures
###################################################
influence.measures(nelson.lm)


###################################################
### chunk number 56: NelsonSummary
###################################################
summary(nelson.lm)


###################################################
### chunk number 57: NelsonCI
###################################################
confint(nelson.lm)


###################################################
### chunk number 58: NelsonPredict
###################################################
predict(nelson.lm, data.frame(HUMIDITY=c(50,100)), interval="prediction", se=T)


###################################################
### chunk number 59: NelsonFig1Hooks
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(1,1))))


###################################################
### chunk number 60: nelsonPlot
###################################################
getOption("SweaveHooks")[["fig"]]()
#create a plot with solid dots (pch=16) and no axis or labels
plot(WEIGHTLOSS~HUMIDITY, data=nelson, pch=16, axes=F, xlab="", ylab="")
#put the x-axis (axis 1) with smaller label font size
axis(1, cex.axis=.8)
#put the x-axis label 3 lines down from the axis
mtext(text="% Relative humidity", side=1, line=3)
#put the y-axis (axis 2) with horizontal tick labels
axis(2, las=1)
#put the y-axis label 3 lines to the left of the axis
mtext(text="Weight loss (mg)", side=2, line=3)
#add the regression line from the fitted model
abline(nelson.lm)
#add the regression formula
text(99,9,"WEIGHTLOSS = -0.053HUMIDITY + 8.704", pos=2)
#add the r squared value
text(99,8.6,expression(paste(r^2==0.975)), pos=2)
#create a sequence of 1000 numbers spanning the range of humidities
x <- seq(min(nelson$HUMIDITY), max(nelson$HUMIDITY),l=1000)
#for each value of x, calculate the upper and lower 95% confidence
y<-predict(nelson.lm, data.frame(HUMIDITY=x), interval="c")
#plot the upper and lower 95% confidence limits
matlines(x,y, lty=1, col=1)
#put an L-shaped box to complete the axis
box(bty="l")


###################################################
### chunk number 61: PeakeReadTable
###################################################
peake <- read.table('peake.csv', header=T, sep=',')


###################################################
### chunk number 62: Temp
###################################################
op <- options(width=40)


###################################################
### chunk number 63: peakeScatterplot1
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
scatterplot(INDIV~AREA, data=peake)


###################################################
### chunk number 64: peakeScatterplot2
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
scatterplot(log10(INDIV)~log10(AREA),
data=peake)


###################################################
### chunk number 65: Temp1
###################################################
options(op)


###################################################
### chunk number 66: PeakeFigHooks
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(2,2))))


###################################################
### chunk number 67: peakeResiduals1
###################################################
getOption("SweaveHooks")[["fig"]]()
peake.lm <- lm(INDIV~AREA, data=peake)
plot(peake.lm)


###################################################
### chunk number 68: peakeResiduals2
###################################################
getOption("SweaveHooks")[["fig"]]()
peake.lm <- lm(log10(INDIV)~log10(AREA), data=peake)
plot(peake.lm)


###################################################
### chunk number 69: PeakeInfluenceMeasures
###################################################
influence.measures(peake.lm)


###################################################
### chunk number 70: PeakeSummary
###################################################
summary(peake.lm)


###################################################
### chunk number 71: PeakeFigHooks1
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(1,1))))


###################################################
### chunk number 72: peakePlot1
###################################################
getOption("SweaveHooks")[["fig"]]()
#create a plot with solid dots (pch=16) and no axis or labels}
plot(INDIV~AREA, data=peake, pch=16, axes=F, xlab="", ylab="",log="xy")
#put the x-axis (axis 1) with smaller label font size
axis(1, cex.axis=.8)
#put the x-axis label 3 lines down from the axis
mtext(text=expression(paste("Mussel clump area", (mm^2))), side=1, line=3)
#put the y-axis (axis 2) with horizontal tick labels
axis(2, las=1)
#put the y-axis label 3 lines to the left of the axis
mtext(text="Number of individuals", side=2, line=3)
#add the regression line from the fitted model
abline(peake.lm)
#add the regression formula
text(30000, 30, expression(paste(log[10], "INDIV = 0.835", log[10],
"AREA - 0.576")), pos=2)
#add the r squared value
text(30000, 22, expression(paste(r^2==0.835)), pos=2)
#put an L-shaped box to complete the axis
box(bty="l")


###################################################
### chunk number 73: PeakePredict
###################################################
10^predict(peake.lm, data.frame(AREA=c(8000,10000)))


###################################################
### chunk number 74: PeakePredict1
###################################################
10^predict(peake.lm,data.frame(AREA=c(8000,10000)), interval="prediction")


###################################################
### chunk number 75: peakePlot2
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(log10(INDIV)~log10(AREA), data=peake, pch=16, axes=F, xlab="", ylab="")
axis(1, cex.axis=.8)
mtext(text="Log Mussel clump area", side=1, line=3)
axis(2, las=1)
mtext(text="Log number of individuals", side=2, line=3)
abline(peake.lm)
text(4.5,1.4,expression(paste(log[10],"INDIV = 0.835",log[10],"AREA - 0.576")), pos=2)
text(4.5,1.3,expression(paste(r^2==0.835)), pos=2)
x <- seq(min(peake$AREA), max(peake$AREA),l=1000)
#x <- seq(1, 30000,l=1000)
y<-predict(peake.lm,data.frame(AREA=x), interval="c")
matlines(log10(x),y, lty=1, col=1)
box(bty="l")


###################################################
### chunk number 76: BeetlesReadTable
###################################################
beetles <- read.table('beetles.csv', header=T, sep=',')


###################################################
### chunk number 77: Temp
###################################################
op <- options(width=40)


###################################################
### chunk number 78: beetlesScatterplot
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
scatterplot(SURVIVAL~DENSITY, 
data=beetles)


###################################################
### chunk number 79: beetlesBoxplots
###################################################
getOption("SweaveHooks")[["fig"]]()
boxplot(SURVIVAL~DENSITY, data=beetles)


###################################################
### chunk number 80: TEMP1
###################################################
options(op)


###################################################
### chunk number 81: BeetlesAnova
###################################################
anova(lm(SURVIVAL~DENSITY + as.factor(DENSITY), beetles))


###################################################
### chunk number 82: BeetlesQF
###################################################
#calculate critical F for alpha=0.25, df=2,11
qf(0.25,2,11, lower=T)


###################################################
### chunk number 83: BeetleSummary
###################################################
beetles.lm <- aov(SURVIVAL~DENSITY+Error(as.factor(DENSITY)), beetles)
summary(beetles.lm)


###################################################
### chunk number 84: BeetlesLM
###################################################
#to get the regression coefficients
lm(SURVIVAL~DENSITY, beetles)


###################################################
### chunk number 85: BeetlesLMSummary
###################################################
summary(lm(SURVIVAL~DENSITY, beetles))


###################################################
### chunk number 86: BeetlesPolynomials
###################################################
beetles$DENSITY<-as.factor(beetles$DENSITY)
contrasts(beetles$DENSITY)<-contr.poly(4,c(5,20,50,100))
beetles.aov<-aov(SURVIVAL~DENSITY, beetles)
summary(beetles.aov,split=list(DENSITY=list(1,c(2,3))))


###################################################
### chunk number 87: ChristReadTable
###################################################
christ <- read.table('christ.csv', header=T, sep=',')


###################################################
### chunk number 88: christScatterplot
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
scatterplot(CWD.BASA~RIP.DENS, data=christ)


###################################################
### chunk number 89: ChristLMII
###################################################
library(biology)
christ.lm <- lm.II(CWD.BASA~RIP.DENS, christ, type="RMA")
summary(christ.lm)


###################################################
### chunk number 90: christPlot
###################################################
getOption("SweaveHooks")[["fig"]]()
#create a plot with solid dots (pch=16) and no axis or labels
plot(CWD.BASA~RIP.DENS, christ, pch=16, axes=F, xlab="", ylab="")
#put the x-axis (axis 1) with smaller label font size
axis(1, cex.axis=.8)
#put the x-axis label 3 lines down from the axis
mtext(text="Riparian tree density", side=1, line=3)
#put the y-axis (axis 2) with horizontal tick labels
axis(2, las=1)
#put the y-axis label 3 lines to the left of the axis
mtext(text="Course woody debris basal area", side=2, line=3)
#add the regression line from the fitted model
abline(christ.lm)
#add the regression parameters
text(1600,50,expression(paste(beta[1]==0.145)), pos=4)
text(1600,40,expression(paste(beta[0]==-113.904)), pos=4)
#put an L-shaped box to complete the axis
box(bty="l")


###################################################
### chunk number 91: SmithReadTable
###################################################
smith <- read.table('smith.csv', header=T, sep=',')


###################################################
### chunk number 92: smithScatterplot
###################################################
getOption("SweaveHooks")[["fig"]]()
scatterplot(RATIO~YEARS,smith)


###################################################
### chunk number 93: SmithMblm
###################################################
library(mblm)
smith.mblm<-mblm(RATIO~YEARS, smith, repeated=F)
summary(smith.mblm)


###################################################
### chunk number 94: SmithCI
###################################################
confint.mblm(smith.mblm, level=0.95)


###################################################
### chunk number 95: McKechnieReadTable
###################################################
mckechnie <- read.table('mckechnie.csv', header=T, sep=',')


###################################################
### chunk number 96: McKechnieRandStat
###################################################
stat<-function(data,index) {
summary(lm(HK~ALT, data))$coef[2,3]
}


###################################################
### chunk number 97: McKechnieRandGen
###################################################
rand.gen<- function(data,mle) {
out<-data
out$ALT <-sample(out$ALT, replace=F)
out
}


###################################################
### chunk number 98: McKechnieBoot
###################################################
library(boot)


###################################################
### chunk number 99: McKechnieBoot
###################################################
mckechnie.boot<-boot(mckechnie, stat, R=5000, sim="parametric", ran.gen=rand.gen)


###################################################
### chunk number 100: mckechnieRandomization
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(mckechnie.boot)


###################################################
### chunk number 101: McKechnieBoot1
###################################################
mckechnie.boot


###################################################
### chunk number 102: McKechnieT
###################################################
t<-length(mckechnie.boot$t[mckechnie.boot$t >= mckechnie.boot$t0])+1
t/(mckechnie.boot$R+1)


###################################################
### chunk number 103: McKechnieCoef
###################################################
par.boot<-function(mckechnie,index) {
x<-mckechnie$ALT[index]
y<-mckechnie$HK[index]
model<-lm(y~x)
coef(model)
}


###################################################
### chunk number 104: McKechnieBoot3
###################################################
mckechnie.boot<-boot(mckechnie, par.boot, R=5000)


###################################################
### chunk number 105: McKechnieBoot4
###################################################
mckechnie.boot


###################################################
### chunk number 106: McKechnieCI1
###################################################
boot.ci(mckechnie.boot, index=2)


###################################################
### chunk number 107: McKechnieBoot5
###################################################
pred.boot<-function(mckechnie,index) {
mckechnie.rs<-mckechnie[index,]
mckechnie.lm<-lm(HK~ALT, mckechnie.rs)
predict(mckechnie.lm, data.frame(ALT=1))
}


###################################################
### chunk number 108: McKechnieBoot6
###################################################
mckechnie.boot<-boot(mckechnie, pred.boot, R=5000)


###################################################
### chunk number 109: McKechnieBoot7
###################################################
mckechnie.boot


###################################################
### chunk number 110: McKechnieCI4
###################################################
boot.ci(mckechnie.boot, index=1)


###################################################
### chunk number 111: PowerAnalysisRegression
###################################################
library(pwr)
pwr.r.test(r=0.5,power=0.99)


###################################################
### chunk number 112: correlationPowerPlot
###################################################
getOption("SweaveHooks")[["fig"]]()
library(pwr)
r<-seq(.4,.9,l=100)
plot(sapply(r,function(x) pwr.r.test(r=x, power=.80)$n)~r, type='l', lwd=2, 
xlab="Correlation coefficient", ylab="Sample size")
points(sapply(r,function(x) pwr.r.test(r=x, power=.90)$n)~r, type='l')
points(sapply(r,function(x) pwr.r.test(r=x, power=.85)$n)~r, type='l')
points(sapply(r,function(x) pwr.r.test(r=x, power=.75)$n)~r, type='l')


###################################################
### chunk number 113: CreateRFile
###################################################
Stangle("../../correlationRegression.rnw")


