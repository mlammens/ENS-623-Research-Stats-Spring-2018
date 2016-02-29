
##equal spread
set.seed(1)
old_par<- par(mar=c(1.5,1.5,0,0))
x <- seq(1,10,by=1)
y <- replicate(1,rnorm(10,x,.5))
yy <- x
plot(x,y, asp=1,axes=F, xlab="", ylab="", lwd=2, xlim=c(0,12),type="n",ylim=c(-2,12),pch=16, cex=1.5)
axis(1, tick=T,tcl=0,lab=F,pos=-2,cex.axis=1, lwd=2, at=seq(0,12,b=1))
mtext("Predictor",1, font=2,cex=1.5)
axis(2, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2,at=seq(-2,12,b=1))
mtext("Response",2, font=2,cex=1.5)
l <- lm(yy~x)
abline(l, col="red",lty=1, lwd=2)
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
points(x,y,col="blue", pch=16, cex=1.5)
boxplot(yy, outline=F,add=T, at=12,axes=F, border="black")
par(old_par)

if(1==2){
#residual plot
old_par<- par(mar=c(1.5,1.5,0,0))
resid <- l$resid
#y <- replicate(1,rnorm(10,x,2))
#yy <- x
plot(py,resid, axes=F, xlab="", ylab="", lwd=2, xlim=c(0,14),type="n",ylim=c(-4,4),pch=16, cex=1.5)
axis(1, tick=T,tcl=0,lab=F,pos=-4,cex.axis=1, lwd=2, at=seq(0,14,b=1))
mtext("Predicted (expected) value",1, font=2,cex=1.5)
axis(2, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2,at=seq(-4,4,b=1))
mtext("Residual",2, font=2,cex=1.5)
points(py,resid,col="blue", pch=16, cex=1.5)
#boxplot(y, outline=F,add=T, at=12,axes=F, border="black")
lines(c(0,14),c(0,0), lty=2)
par(old_par)


#skewed
old_par<- par(mar=c(1.5,1.5,0,0))
x <- seq(1,10,by=1)
y <- c(1.1,1.7,3.7,3.5,6,5.2,8.6,7.1,11.1,8)
yy <- x
plot(x,y, asp=1,axes=F, xlab="", ylab="", lwd=2, xlim=c(0,14),type="n",ylim=c(-2,14),pch=16, cex=1.5)
axis(1, tick=T,tcl=0,lab=F,pos=-2,cex.axis=1, lwd=2, at=seq(0,14,b=1))
mtext("Predictor",1, font=2,cex=1.5)
axis(2, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2,at=seq(-2,14,b=1))
mtext("Response",2, font=2,cex=1.5)
l <- lm(y~x)
abline(l, col="red",lty=1, lwd=2)
y1 <- replicate(1,rnorm(10000,1,.1))
y2 <- replicate(1,rnorm(10000,2,.2))
y3 <- replicate(1,rnorm(10000,3,.3))
y4 <- replicate(1,rnorm(10000,4,.4))
y5 <- replicate(1,rnorm(10000,5,.5))
y6 <- replicate(1,rnorm(10000,6,.6))
y7 <- replicate(1,rnorm(10000,7,.7))
y8 <- replicate(1,rnorm(10000,8,.8))
y9 <- replicate(1,rnorm(10000,9,.9))
y10 <- replicate(1,rnorm(10000,10,1))
#points(rep(x,times=10),replicate(10,rnorm(10,x,1)), col="gray",pch=16)
#boxplot(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10, outline=F,add=T, axes=F, border="black")
#boxplot(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10, outline=F,add=T, axes=F, border="gray")
points(x,y,col="blue", pch=16, cex=1.5)
boxplot(y, outline=F,add=T, at=12,axes=F, border="black")
py<-(l$coef[2]*x)+l$coef[1]
segments(x,y,x,(l$coef[2]*x)+l$coef[1], lwd=2,col="black", lty=3)
par(old_par)


#requires above one first
windows(height=4,width=4)
old_par<- par(mar=c(1.5,1.5,0,0))
resid <- l$resid
#y <- replicate(1,rnorm(10,x,2))
#yy <- x
plot(py,resid, axes=F, xlab="", ylab="", lwd=2, xlim=c(0,14),type="n",ylim=c(-4,4),pch=16, cex=1.5)
axis(1, tick=T,tcl=0,lab=F,pos=-4,cex.axis=1, lwd=2, at=seq(0,14,b=1))
mtext("Predicted (expected) value",1, font=2,cex=1.5)
axis(2, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2,at=seq(-4,4,b=1))
mtext("Residual",2, font=2,cex=1.5)
points(py,resid,col="blue", pch=16, cex=1.5)
#boxplot(y, outline=F,add=T, at=12,axes=F, border="black")
lines(c(0,14),c(0,0), lty=2)
lines(c(0,14),c(0,4), col="red",lty=2)
lines(c(0,14),c(0,-4), col="red",lty=2)

par(old_par)
}

set.seed(3)
old_par<- par(mar=c(1.5,1.5,0,0))
x <- seq(1,10,by=1)
y <- replicate(1,rlnorm(10,log(x),.5^2))
yy <- x
plot(x,y, asp=1,axes=F, xlab="", ylab="", lwd=2, xlim=c(0,12),type="n",ylim=c(-2,16),pch=16, cex=1.5)
axis(1, tick=T,tcl=0,lab=F,pos=-2,cex.axis=1, lwd=2, at=seq(0,14,b=1))
mtext("Predictor",1, font=2,cex=1.5)
axis(2, tick=T,tcl=0,lab=F,pos=0,cex.axis=1, lwd=2,at=seq(-2,16,b=1))
mtext("Response",2, font=2,cex=1.5)
l <- lm(yy~x)
abline(l, col="red",lty=1, lwd=2)
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
points(x,y,col="blue", pch=16, cex=1.5)
boxplot(rlnorm(1000,log(x),.5^2), outline=F,add=T, at=12,axes=F, border="black", width=2)

par(old_par)
