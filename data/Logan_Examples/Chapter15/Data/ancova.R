###################################################
### chunk number 1: Ancova_SetDir
###################################################
setwd("Ancova/Data")
setCacheDir("../cache")
options(SweaveHooks=list(fig=function() par(mar=c(4,4,0.1,0.1))) )


###################################################
### chunk number 2: AncovaDiagram1
###################################################
getOption("SweaveHooks")[["fig"]]()
#postscript(file="AncovaDiagram1.ps", width=7, height=9,paper='special', horizontal=FALSE)
 source("../../Figure_helper.R")   
    old <- par(mfrow=c(4,2),mar=c(2,3,0.1,0.1),xpd=FALSE)
    { #Figure a ================================== 
        set.seed(3) #(7)
        A.iv <-rnorm(10,20,15)
        A.dv <-(-.75*A.iv)+40+rnorm(10,0,4)
        mean.A.iv <- mean(A.iv)
        
        B.iv<-rnorm(10,22.5,15)
        B.dv<-(-.75*B.iv)+25+rnorm(10,0,4)
        mean.B.iv <- mean(B.iv)
        
        C.iv<-rnorm(10,20,15)
        C.dv<-(-.75*C.iv)+20+rnorm(10,0,4)
        mean.C.iv <- mean(C.iv)
        
        total.dv<-c(A.iv,B.iv,C.iv)
        total.iv <- c(A.dv,B.dv,C.dv)
        mean.Total.iv <- mean(total.dv)
        mean.Total.dv <- mean(total.iv)
        
        ## print(Anova(aov(total.dv~total.iv+total.fact))) 
        ## print(Anova(aov(total.dv~total.fact)))
        
        plot(c(A.dv,B.dv,C.dv)~c(A.iv,B.iv,C.iv), xlim=c(0,4), type="n", axes=FALSE, xlab="",ylab="")
        points(A.dv~seq(.75,1.25,l=10),pch=16)
        points(B.dv~seq(1.75,2.25,l=10),pch=21)
        points(C.dv~seq(2.75,3.25,l=10),pch=19, col="gray", bg="white")
        points(C.dv~seq(2.75,3.25,l=10),pch=21, col="black")

                                        #mean of group A
        mean.A.dv <- mean(A.dv)
        lines(seq(.65,1.35,l=10), rep(mean.A.dv,10), lwd=2)
        arrows(seq(.75,1.25,l=10), rep(mean.A.dv,10),seq(.75,1.25,l=10), A.dv, lty=1, code=1, length=.01) 
        
                                        #mean of group B
        mean.B.dv <- mean(B.dv)
        lines(seq(1.65,2.35,l=10), rep(mean.B.dv,10), lwd=2)
        arrows(seq(1.75,2.25,l=10), rep(mean.B.dv,10),seq(1.75,2.25,l=10), B.dv, lty=1, code=1, length=.01) 
        
                                        #mean of group C
        mean.C.dv <- mean(C.dv)
        lines(seq(2.65,3.35,l=10), rep(mean.C.dv,10), lwd=2)
        arrows(seq(2.75,3.25,l=10), rep(mean.C.dv,10),seq(2.75,3.25,l=10), C.dv, lty=1, code=1, length=.01)
        
        axis(1, at=c(1,2, 3),lab=c('Group A','Group B','Group C'),mgp=c(0,.5,0))
        axis(2,labels=FALSE,tick=FALSE)
        mtext("Response",2, line=1.5)
                                        #explained mean distance
                                        #segments(.3,10+dist.a1/200,.3,10-dist.a1/200)
                                        #segments(.2,10+dist.total1/200,.2,10-dist.total1/200, lty=2)
        text(upperLeft(offset=0.05),"a)", cex=2)
        box(bty="o")
        
                                        #anova(aov(c(dv,dv1)~factor(c(rep("A",10),rep("B",10)))))
      }
    { #Figure b ========================================================
      library(car)
      total.dv<-c(A.dv,B.dv,C.dv)
      total.iv <- c(A.iv,B.iv,C.iv)
      total.fact <- factor(c(rep("A",10),rep("B",10),rep("C",10)))
      
      mean.total.iv <- mean(total.iv)
      
                                        #generate the plot
      plot(total.dv~total.iv, type="n", axes=FALSE, xlab="", ylab="")
      
                                        #calculate the aggregate slope
      nXa <- A.iv-mean.A.iv
      nXb <- B.iv-mean.B.iv
      nXc <- C.iv-mean.C.iv
      b<-coef(lm(total.dv~c(nXa,nXb,nXc)))[2]
      a <- mean.Total.dv-(b*mean.Total.iv) #coef(lm(total.dv~c(nXa,nXb,nXc)))[1]
      diff.a <- b*(mean.A.iv-mean.Total.iv)
      diff.b <- b*(mean.B.iv-mean.Total.iv)
      diff.c <- b*(mean.C.iv-mean.Total.iv)
      (adj.mean.A.dv <- mean.A.dv-diff.a)
      (adj.mean.B.dv <- mean.B.dv-diff.b)
      (adj.mean.C.dv <- mean.C.dv-diff.c)
      diff.aa <-adj.mean.A.dv-mean.Total.dv
      diff.bb <-adj.mean.B.dv-mean.Total.dv
      diff.cc <-adj.mean.C.dv-mean.Total.dv
      
                                        #Group A
      points(A.dv~A.iv, pch=16)
      xs <- seq(min(A.iv),max(A.iv),l=1000)
      points(xs, (b*xs)+diff.aa+a, type="l", lty=1)
      arrows(A.iv,A.dv,A.iv,(b*A.iv)+diff.aa+a,code=0)
                                        #points(mean.A.iv,adj.mean.A.dv, col="red")
                                        #points(mean.A.iv,mean.A.dv, col="blue")
      
      
                                        #Group B
      points(B.dv~B.iv, pch=21)
      xs <- seq(min(B.iv),max(B.iv),l=1000)
      points(xs, (b*xs)+diff.bb+a, type="l", lty=1)
      arrows(B.iv,B.dv,B.iv,(b*B.iv)+diff.bb+a,code=0)
                                        #points(xs, predict(total.lm,data.frame(total.iv=xs))+adj.a, type="l", lty=1)
      
                                        #Group C
      points(C.dv~C.iv, pch=19, col="gray")
      points(C.dv~C.iv, pch=21, col="black")
      xs <- seq(min(C.iv),max(C.iv),l=1000)
      points(xs, (b*xs)+diff.cc+a, type="l", lty=1)
      arrows(C.iv,C.dv,C.iv,(b*C.iv)+diff.cc+a,code=0)
                                        #points(xs, predict(total.lm,data.frame(total.iv=xs))+adj.a, type="l", lty=1)
      legend("topright",legend=c("Group A", "Group B", "Group C"), pch=c(16,21,21),pt.bg=c("black","white", "gray"),col=1,bty="n")
      mtext("Covariate",1, line=.5)
      text(upperLeft(offset=0.05),"b)", cex=2)
      box(bty="o")
    }
    { #Figure c =======================================================
      set.seed(3)
      par(mar=c(2,3,0.1,2))
      A.iv <-rnorm(10,10,15)
      A.dv <-(-.15*A.iv)+30+rnorm(10,0,1)
      mean.A.iv <- mean(A.iv)
      mean.A.dv <- mean(A.dv)
      
      B.iv<-rnorm(10,35,15)
      B.dv<-(-.15*B.iv)+10+rnorm(10,0,1)
      mean.B.iv <- mean(B.iv)
      mean.B.dv <- mean(B.dv)
      
      C.iv<-rnorm(10,40,15)
      C.dv<-(-.15*C.iv)+0+rnorm(10,0,1)
      mean.C.iv <- mean(C.iv)
      mean.C.dv <- mean(C.dv)
      
      dd<-mean.A.dv-mean.B.dv
      A.dv<-A.dv-dd-.65
      mean.A.dv <- mean(A.dv)
      dd<-mean.C.dv-mean.B.dv
      C.dv<-C.dv-dd+.5
      mean.C.dv <- mean(C.dv)
      
      C.dv <- rep(NA,10)
      C.iv <- rep(NA,10)
      
      mean.Total.iv <- mean(c(A.iv,B.iv,C.iv), na.rm=T)
      mean.Total.dv <- mean(c(A.dv,B.dv,C.dv), na.rm=T)
      
      
      total.dv<-c(A.dv,B.dv,C.dv)
      total.iv <- c(A.iv,B.iv,C.iv)
      total.fact <- factor(c(rep("A",10),rep("B",10),rep("C",10)))
      #print(Anova(aov(total.dv~total.iv+total.fact)))
      #print(Anova(aov(total.dv~total.fact)))
      
      ylim <- c(min(total.dv,na.rm=T)-1, max(total.dv,na.rm=T)+1)
      plot(c(A.dv,B.dv,C.dv)~c(A.iv,B.iv,C.iv), xlim=c(0,3),ylim=ylim, type="n", axes=FALSE, xlab="",ylab="")
      points(A.dv~seq(.75,1.25,l=10),pch=16)
      points(B.dv~seq(1.75,2.25,l=10),pch=21)
      points(C.dv~seq(2.75,3.25,l=10),pch=19, col="gray", bg="white")
      points(C.dv~seq(2.75,3.25,l=10),pch=21, col="black")
      
                                        #calculate the aggregate slope
      nXa <- A.iv-mean.A.iv
      nXb <- B.iv-mean.B.iv
      nXc <- C.iv-mean.C.iv
      b<-coef(lm(total.dv~c(nXa,nXb,nXc)))[2]
      a <- mean.Total.dv-(b*mean.Total.iv) #coef(lm(total.dv~c(nXa,nXb,nXc)))[1]
      diff.a <- b*(mean.A.iv-mean.Total.iv)
      diff.b <- b*(mean.B.iv-mean.Total.iv)
      diff.c <- b*(mean.C.iv-mean.Total.iv)
      (adj.mean.A.dv <- mean.A.dv-diff.a)
      (adj.mean.B.dv <- mean.B.dv-diff.b)
      (adj.mean.C.dv <- mean.C.dv-diff.c)
      
      axis(4,
           at=c(mean.A.dv,mean.B.dv, adj.mean.A.dv,adj.mean.B.dv),
           lab=c("","","",""),
           las=2)
      
                                        #mean of group A
      mean.A.dv <- mean(A.dv)
      lines(seq(.65,1.35,l=10), rep(mean.A.dv,10), lwd=2)
      arrows(seq(.75,1.25,l=10), rep(mean.A.dv,10),seq(.75,1.25,l=10), A.dv, lty=1, code=1, length=.01, col="gray") 
      lines(c(1,100),c(mean.A.dv,mean.A.dv),lty=2)
      
                                        #mean of group B
      mean.B.dv <- mean(B.dv)
      lines(seq(1.65,2.35,l=10), rep(mean.B.dv,10), lwd=2)
      arrows(seq(1.75,2.25,l=10), rep(mean.B.dv,10),seq(1.75,2.25,l=10), B.dv, lty=1, code=1, length=.01, col="gray") 
      lines(c(2,100),c(mean.B.dv,mean.B.dv),lty=2)
      
                                        #mean of group C
      mean.C.dv <- mean(C.dv,na.rm=T)
      lines(seq(2.65,3.35,l=10), rep(mean.C.dv,10), lwd=2)
      arrows(seq(2.75,3.25,l=10), rep(mean.C.dv,10),seq(2.75,3.25,l=10), C.dv, lty=1, code=1, length=.01)
      lines(c(3,100),c(mean.C.dv,mean.C.dv),lty=2)
      
                                        #axis(1, at=c(1,2, 3),lab=c('Group A','Group B','Group C'))
      axis(1, at=c(1,2),lab=c('Group A','Group B'), mgp=c(0,.5,0))
      axis(2,labels=FALSE,tick=FALSE)
                                        #explained mean distance
                                        #segments(.3,10+dist.a1/200,.3,10-dist.a1/200)
                                        #segments(.2,10+dist.total1/200,.2,10-dist.total1/200, lty=2)
      mtext("Response",2, line=1.5)
      text(upperLeft(offset=0.05),"c)", cex=2)
      box(bty="o")
    }
    { #plot d================================================
      library(car)
      #print(par()$mar)
      par(mar=c(2,3,0.1,0.1))
      total.dv<-c(A.dv,B.dv,C.dv)
      total.iv <- c(A.iv,B.iv,C.iv)
      total.fact <- factor(c(rep("A",10),rep("B",10),rep("C",10)))
      
      mean.total.iv <- mean(total.iv,na.rm=T)
      
                                        #generate the plot
      plot(total.dv~total.iv, ylim=ylim,type="n", axes=FALSE, xlab="", ylab="")
      
                                        #calculate the aggregate slope
      nXa <- A.iv-mean.A.iv
      nXb <- B.iv-mean.B.iv
      nXc <- C.iv-mean.C.iv
      b<-coef(lm(total.dv~c(nXa,nXb,nXc)))[2]
      a <- mean.Total.dv-(b*mean.Total.iv) #coef(lm(total.dv~c(nXa,nXb,nXc)))[1]
      diff.a <- b*(mean.A.iv-mean.Total.iv)
      diff.b <- b*(mean.B.iv-mean.Total.iv)
      diff.c <- b*(mean.C.iv-mean.Total.iv)
      (adj.mean.A.dv <- mean.A.dv-diff.a)
      (adj.mean.B.dv <- mean.B.dv-diff.b)
      (adj.mean.C.dv <- mean.C.dv-diff.c)
      diff.aa <-adj.mean.A.dv-mean.Total.dv
      diff.bb <-adj.mean.B.dv-mean.Total.dv
      diff.cc <-adj.mean.C.dv-mean.Total.dv
      
      lines(c(mean.Total.iv, mean.Total.iv),ylim, col="black", lty=2)
      xs <- seq(min(total.iv,na.rm=T),max(total.iv,na.rm=T),l=1000)
      points(xs, (b*xs)+a, type="l", lty=1, col="gray")
      
      axis(1, at=c(mean.A.iv,mean.Total.iv,mean.B.iv),
           lab=c(expression(paste(bar(X)[A])),expression(paste(bar(X))),
             expression(paste(bar(X)[B]))),mgp=c(0,0.75,0))
      axis(2,
           at=c(mean.A.dv,mean.B.dv,adj.mean.A.dv,adj.mean.B.dv),
           lab=c(expression(paste(,"   ",bar(Y)[A],"    ")),
             expression(paste(bar(Y)[B],"    ")),
             expression(paste(bar(Y)[A(adj)])),expression(paste(bar(Y)[B(adj)]))),
           las=2)
                                        #Group A
      points(A.dv~A.iv, pch=16)
      xs <- seq(min(A.iv),max(A.iv),l=1000)
      points(xs, (b*xs)+diff.aa+a, type="l", lty=1)
      arrows(A.iv,A.dv,A.iv,(b*A.iv)+diff.aa+a,code=0, col="gray")
                                        #points(mean.A.iv,adj.mean.A.dv, col="red")
                                        #points(mean.A.iv,mean.A.dv, col="blue")
      lines(c(mean.Total.iv,-100),c(adj.mean.A.dv,adj.mean.A.dv),lty=2)
      lines(c(mean.A.iv, mean.A.iv),c(-100,(b*mean.A.iv)+diff.aa+a), col="gray", lty=2)
      
                                        #Group B
      points(B.dv~B.iv, pch=21)
      xs <- seq(min(B.iv),max(B.iv),l=1000)
      points(xs, (b*xs)+diff.bb+a, type="l", lty=1)
      arrows(B.iv,B.dv,B.iv,(b*B.iv)+diff.bb+a,code=0, col="gray")
                                        #points(xs, predict(total.lm,data.frame(total.iv=xs))+adj.a, type="l", lty=1)
      lines(c(mean.Total.iv,-100),c(adj.mean.B.dv,adj.mean.B.dv),lty=2)
      lines(c(mean.B.iv, mean.B.iv),c(-100,(b*mean.B.iv)+diff.bb+a), col="gray", lty=2)
      
                                        #Group C
      points(C.dv~C.iv, pch=19, col="gray")
      points(C.dv~C.iv, pch=21, col="black")
      xs <- seq(min(C.iv,na.rm=T),max(C.iv,na.rm=T),l=1000)
      points(xs, (b*xs)+diff.cc+a, type="l", lty=1)
      arrows(C.iv,C.dv,C.iv,(b*C.iv)+diff.cc+a,code=0)
      lines(c(mean.Total.iv,-100),c(adj.mean.C.dv,adj.mean.C.dv),lty=2)
                                        #lines(c(mean.C.iv, mean.C.iv),ylim, col="gray")
      
                                        #points(xs, predict(total.lm,data.frame(total.iv=xs))+adj.a, type="l", lty=1)
                                        #legend("topright",legend=c("Group A", "Group B", "Group C"), pch=c(16,21,21),pt.bg=c("black","white", "gray"),col=1,bty="n")
      legend("topright",legend=c("Group A", "Group B"), pch=c(16,21),pt.bg=c("black","white"),col=1,bty="n")
      
      text(upperLeft(offset=0.05),"d)", cex=2)
      box(bty="o")
    }
    { #plot e ============================================================
      set.seed(4)
      par(mar=c(2,3,0.1,.1))
      A.iv <-rnorm(10,10,15)
      A.dv <-(-.15*A.iv)+2+rnorm(10,0,1)
      mean.A.iv <- mean(A.iv)
      mean.A.dv <- mean(A.dv)
      
      B.iv<-rnorm(10,10,15)
      B.dv<-(.15*B.iv)+2+rnorm(10,0,1)
      mean.B.iv <- mean(B.iv)
      mean.B.dv <- mean(B.dv)

      #dd<-mean.A.dv-mean.B.dv
      #A.dv<-A.dv-dd-.65
      #mean.A.dv <- mean(A.dv)
      #dd<-mean.C.dv-mean.B.dv
      
      mean.Total.iv <- mean(c(A.iv,B.iv), na.rm=T)
      mean.Total.dv <- mean(c(A.dv,B.dv), na.rm=T)
      
      
      total.dv<-c(A.dv,B.dv)
      total.iv <- c(A.iv,B.iv)
      total.fact <- factor(c(rep("A",10),rep("B",1)))
      
      ylim <- c(min(total.dv,na.rm=T)-1, max(total.dv,na.rm=T)+4)
      plot(total.dv~total.iv, ylim=ylim,type="n", axes=FALSE, xlab="", ylab="")

      nXa <- A.iv-mean.A.iv
      nXb <- B.iv-mean.B.iv
      b<-coef(lm(total.dv~c(nXa,nXb)))[2]
      a <- mean.Total.dv-(b*mean.Total.iv) #coef(lm(total.dv~c(nXa,nXb,nXc)))[1]
      diff.a <- b*(mean.A.iv-mean.Total.iv)
      diff.b <- b*(mean.B.iv-mean.Total.iv)
      (adj.mean.A.dv <- mean.A.dv-diff.a)
      (adj.mean.B.dv <- mean.B.dv-diff.b)
      diff.aa <-adj.mean.A.dv-mean.Total.dv
      diff.bb <-adj.mean.B.dv-mean.Total.dv
      
                                        #Group A
      #lines(c(1,100),c(mean.A.dv,mean.A.dv),lty=2)
      xs <- seq(min(A.iv),max(A.iv),l=1000)
      #points(xs, (b*xs)+diff.aa+a, type="l", lty=1)
      points(A.dv~A.iv, pch=16)
      b1 <- coef(lm(A.dv~A.iv))[2]
      a1 <- coef(lm(A.dv~A.iv))[1]
      points(xs, (b1*xs)+a1, type="l", lty=1, col="black")
      #arrows(A.iv,A.dv,A.iv,(b*A.iv)+diff.aa+a,code=0, col="gray")

                                        #Group B
      xs <- seq(min(B.iv),max(B.iv),l=1000)
      #points(xs, (b*xs)+diff.bb+a, type="l", lty=2)
      points(B.dv~B.iv, pch=21)
      b2 <- coef(lm(B.dv~B.iv))[2]
      a2 <- coef(lm(B.dv~B.iv))[1]
      points(xs, (b2*xs)+a2, type="l", lty=1, col="black")
      #arrows(B.iv,B.dv,B.iv,(b*B.iv)+diff.bb+a,code=0, col="gray")

      axis(2,labels=FALSE,tick=FALSE)
      mtext("Response",2, line=1.5)
      mtext("Covariate",1, line=.5)
      text(upperLeft(offset=0.05),"e)", cex=2)
      legend("topright",legend=c("Group A", "Group B"), pch=c(16,21),pt.bg=c("black","white"),col=1,bty="n")
      
      box(bty="o")

    }
    { #plot f ============================================================
      set.seed(4)
      par(mar=c(2,3,0.1,.1))
      A.iv <-rnorm(10,10,15)
      A.dv <-(-.15*A.iv)+2+rnorm(10,0,1)
      mean.A.iv <- mean(A.iv)
      mean.A.dv <- mean(A.dv)
      
      B.iv<-rnorm(10,10,15)
      B.dv<-(.15*B.iv)+2+rnorm(10,0,1)
      mean.B.iv <- mean(B.iv)
      mean.B.dv <- mean(B.dv)

      #dd<-mean.A.dv-mean.B.dv
      #A.dv<-A.dv-dd-.65
      #mean.A.dv <- mean(A.dv)
      #dd<-mean.C.dv-mean.B.dv
      
      mean.Total.iv <- mean(c(A.iv,B.iv), na.rm=T)
      mean.Total.dv <- mean(c(A.dv,B.dv), na.rm=T)
      
      
      total.dv<-c(A.dv,B.dv)
      total.iv <- c(A.iv,B.iv)
      total.fact <- factor(c(rep("A",10),rep("B",1)))
      
      ylim <- c(min(total.dv,na.rm=T)-1, max(total.dv,na.rm=T)+4)
      plot(total.dv~total.iv, ylim=ylim,type="n", axes=FALSE, xlab="", ylab="")

      nXa <- A.iv-mean.A.iv
      nXb <- B.iv-mean.B.iv
      b<-coef(lm(total.dv~c(nXa,nXb)))[2]
      a <- mean.Total.dv-(b*mean.Total.iv) #coef(lm(total.dv~c(nXa,nXb,nXc)))[1]
      diff.a <- b*(mean.A.iv-mean.Total.iv)
      diff.b <- b*(mean.B.iv-mean.Total.iv)
      (adj.mean.A.dv <- mean.A.dv-diff.a)
      (adj.mean.B.dv <- mean.B.dv-diff.b)
      diff.aa <-adj.mean.A.dv-mean.Total.dv
      diff.bb <-adj.mean.B.dv-mean.Total.dv

      #lines(c(mean.Total.iv, mean.Total.iv),ylim, col="black", lty=2)
      xs <- seq(min(total.iv,na.rm=T),max(total.iv,na.rm=T),l=1000)
      #points(xs, (b*xs)+a, type="l", lty=1, col="gray")
      
                                        #Group A
      #lines(c(mean.Total.iv,-100),c(adj.mean.A.dv,adj.mean.A.dv),lty=2)
      #lines(c(mean.A.iv, mean.A.iv),c(-100,(b*mean.A.iv)+diff.aa+a), col="gray", lty=2)
      xs <- seq(min(A.iv),max(A.iv),l=1000)
      points(xs, (b*xs)+diff.aa+a, type="l", lty=1)
      points(A.dv~A.iv, pch=16)
      b1 <- coef(lm(A.dv~A.iv))[2]
      a1 <- coef(lm(A.dv~A.iv))[1]
      #points(xs, (b1*xs)+a1, type="l", lty=1, col="gray")
      arrows(A.iv,A.dv,A.iv,(b*A.iv)+diff.aa+a,code=0, col="gray")
      #arrows(A.iv,A.dv,A.iv,(b*A.iv)+diff.aa+a,code=0, col="gray")

                                        #Group B
      #lines(c(mean.Total.iv,-100),c(adj.mean.B.dv,adj.mean.B.dv),lty=2)
      xs <- seq(min(B.iv),max(B.iv),l=1000)
      points(xs, (b*xs)+diff.bb+a, type="l", lty=1)
      points(B.dv~B.iv, pch=21)
      b2 <- coef(lm(B.dv~B.iv))[2]
      a2 <- coef(lm(B.dv~B.iv))[1]
      #points(xs, (b2*xs)+a2, type="l", lty=2, col="gray")
      arrows(B.iv,B.dv,B.iv,(b*B.iv)+diff.bb+a,code=0, col="gray")

      axis(2,labels=FALSE,tick=FALSE)
      #mtext("Response",2, line=1.5)
      mtext("Covariate",1, line=.5)
      text(upperLeft(offset=0.05),"f)", cex=2)
      legend("topright",legend=c("Group A", "Group B"), pch=c(16,21),pt.bg=c("black","white"),col=1,bty="n")
      
      box(bty="o")

    }
    { #plot g ============================================================
      set.seed(4)
      par(mar=c(2,4,0.1,1))
      A.iv <-rnorm(10,10,15)
      A.dv <-(.15*A.iv)+2+rnorm(10,0,1)
      mean.A.iv <- mean(A.iv)
      mean.A.dv <- mean(A.dv)
      
      B.iv<-rnorm(10,50,15)
      B.dv<-(.15*B.iv)+4+rnorm(10,0,1)
      mean.B.iv <- mean(B.iv)
      mean.B.dv <- mean(B.dv)

      #dd<-mean.A.dv-mean.B.dv
      #A.dv<-A.dv-dd-.65
      #mean.A.dv <- mean(A.dv)
      #dd<-mean.C.dv-mean.B.dv
      
      mean.Total.iv <- mean(c(A.iv,B.iv), na.rm=T)
      mean.Total.dv <- mean(c(A.dv,B.dv), na.rm=T)
      
      
      total.dv<-c(A.dv,B.dv)
      total.iv <- c(A.iv,B.iv)
      total.fact <- factor(c(rep("A",10),rep("B",1)))
      
      ylim <- c(min(total.dv,na.rm=T)-1, max(total.dv,na.rm=T)+4)
      plot(total.dv~total.iv, ylim=ylim,type="n", axes=FALSE, xlab="", ylab="")

      nXa <- A.iv-mean.A.iv
      nXb <- B.iv-mean.B.iv
      b<-coef(lm(total.dv~c(nXa,nXb)))[2]
      a <- mean.Total.dv-(b*mean.Total.iv) #coef(lm(total.dv~c(nXa,nXb,nXc)))[1]
      diff.a <- b*(mean.A.iv-mean.Total.iv)
      diff.b <- b*(mean.B.iv-mean.Total.iv)
      (adj.mean.A.dv <- mean.A.dv-diff.a)
      (adj.mean.B.dv <- mean.B.dv-diff.b)
      diff.aa <-adj.mean.A.dv-mean.Total.dv
      diff.bb <-adj.mean.B.dv-mean.Total.dv

      lines(c(mean.Total.iv, mean.Total.iv),ylim, col="black", lty=2)
                                        #Group A
      #lines(c(1,100),c(mean.A.dv,mean.A.dv),lty=2)
      lines(c(mean.A.iv,-100),c(mean.A.dv,mean.A.dv),lty=2, col="gray")
      lines(c(mean.Total.iv,-100),c(adj.mean.A.dv,adj.mean.A.dv),lty=2)
      xs <- seq(min(A.iv),max(A.iv),l=1000)
      #points(xs, (b*xs)+diff.aa+a, type="l", lty=1)
      points(A.dv~A.iv, pch=16)
      b1 <- coef(lm(A.dv~A.iv))[2]
      a1 <- coef(lm(A.dv~A.iv))[1]
      points(xs, (b1*xs)+a1, type="l", lty=1, col="black")
      #arrows(A.iv,A.dv,A.iv,(b*A.iv)+diff.aa+a,code=0, col="gray")

                                        #Group B
      lines(c(mean.B.iv,-100),c(mean.B.dv,mean.B.dv),lty=2, col="gray")
      #lines(c(1,100),c(mean.B.dv,mean.B.dv),lty=2)
      lines(c(mean.Total.iv,-100),c(adj.mean.B.dv,adj.mean.B.dv),lty=2)
      xs <- seq(min(B.iv),max(B.iv),l=1000)
      #points(xs, (b*xs)+diff.bb+a, type="l", lty=2)
      points(B.dv~B.iv, pch=21)
      b2 <- coef(lm(B.dv~B.iv))[2]
      a2 <- coef(lm(B.dv~B.iv))[1]
      points(xs, (b2*xs)+a2, type="l", lty=1, col="black")
      #arrows(B.iv,B.dv,B.iv,(b*B.iv)+diff.bb+a,code=0, col="gray")

      axis(2,labels=FALSE,tick=FALSE)
      #mtext("Response",2, line=1.5)
      axis(2,
           at=c(mean.A.dv,mean.B.dv,adj.mean.A.dv,adj.mean.B.dv),
           lab=c(expression(paste(,"   ",bar(Y)[A],"    ")),
             expression(paste(bar(Y)[B],"    ")),
             expression(paste(bar(Y)[A(adj)])),expression(paste(bar(Y)[B(adj)]))),
           las=2)
      mtext("Covariate",1, line=.5)
      text(upperLeft(offset=0.05),"g)", cex=2)
      legend("topright",legend=c("Group A", "Group B"), pch=c(16,21),pt.bg=c("black","white"),col=1,bty="n")
      
      box(bty="o")

    }
    { #plot h ============================================================
      set.seed(4)
      par(mar=c(2,4,0.1,1))

      A.iv <-rnorm(10,10,15)
      A.dv <-(.15*A.iv)+2+rnorm(10,0,1)
      mean.A.iv <- mean(A.iv)
      mean.A.dv <- mean(A.dv)
      
      B.iv<-rnorm(10,50,15)
      B.dv<-(.15*B.iv)-4+rnorm(10,0,1)
      mean.B.iv <- mean(B.iv)
      mean.B.dv <- mean(B.dv)

      #dd<-mean.A.dv-mean.B.dv
      #A.dv<-A.dv-dd-.65
      #mean.A.dv <- mean(A.dv)
      #dd<-mean.C.dv-mean.B.dv
      
      mean.Total.iv <- mean(c(A.iv,B.iv), na.rm=T)
      mean.Total.dv <- mean(c(A.dv,B.dv), na.rm=T)
      
      
      total.dv<-c(A.dv,B.dv)
      total.iv <- c(A.iv,B.iv)
      total.fact <- factor(c(rep("A",10),rep("B",1)))
      
      ylim <- c(min(total.dv,na.rm=T)-1, max(total.dv,na.rm=T)+4)
      plot(total.dv~total.iv, ylim=ylim,type="n", axes=FALSE, xlab="", ylab="")

      nXa <- A.iv-mean.A.iv
      nXb <- B.iv-mean.B.iv
      b<-coef(lm(total.dv~c(nXa,nXb)))[2]
      a <- mean.Total.dv-(b*mean.Total.iv) #coef(lm(total.dv~c(nXa,nXb,nXc)))[1]
      diff.a <- b*(mean.A.iv-mean.Total.iv)
      diff.b <- b*(mean.B.iv-mean.Total.iv)
      (adj.mean.A.dv <- mean.A.dv-diff.a)
      (adj.mean.B.dv <- mean.B.dv-diff.b)
      diff.aa <-adj.mean.A.dv-mean.Total.dv
      diff.bb <-adj.mean.B.dv-mean.Total.dv

      lines(c(mean.Total.iv, mean.Total.iv),ylim, col="black", lty=2)
      xs <- seq(min(total.iv,na.rm=T),max(total.iv,na.rm=T),l=1000)
      #points(xs, (b*xs)+a, type="l", lty=1, col="gray")
      
                                        #Group A
      lines(c(mean.Total.iv,-100),c(adj.mean.A.dv,adj.mean.A.dv),lty=2)
      lines(c(mean.A.iv,-100),c(mean.A.dv,mean.A.dv),lty=2, col="gray")
      xs <- seq(min(A.iv),max(A.iv),l=1000)
      points(xs, (b*xs)+diff.aa+a, type="l", lty=1)
      points(A.dv~A.iv, pch=16)
      b1 <- coef(lm(A.dv~A.iv))[2]
      a1 <- coef(lm(A.dv~A.iv))[1]
      #points(xs, (b1*xs)+a1, type="l", lty=1, col="gray")
      arrows(A.iv,A.dv,A.iv,(b*A.iv)+diff.aa+a,code=0, col="gray")
      #arrows(A.iv,A.dv,A.iv,(b*A.iv)+diff.aa+a,code=0, col="gray")

                                        #Group B
      lines(c(mean.B.iv,-100),c(mean.B.dv,mean.A.dv),lty=2, col="gray")
      lines(c(mean.Total.iv,-100),c(adj.mean.B.dv,adj.mean.B.dv),lty=2)
      xs <- seq(min(B.iv),max(B.iv),l=1000)
      points(xs, (b*xs)+diff.bb+a, type="l", lty=1)
      points(B.dv~B.iv, pch=21)
      b2 <- coef(lm(B.dv~B.iv))[2]
      a2 <- coef(lm(B.dv~B.iv))[1]
      #points(xs, (b2*xs)+a2, type="l", lty=2, col="gray")
      arrows(B.iv,B.dv,B.iv,(b*B.iv)+diff.bb+a,code=0, col="gray")

      axis(2,labels=FALSE,tick=FALSE)
      #mtext("Response",2, line=1.5)
      mtext("Covariate",1, line=.5)
      axis(2,
           at=c(mean.A.dv,adj.mean.A.dv,adj.mean.B.dv),
           lab=c(expression(paste(bar(Y)[A]," & ",bar(Y)[B])),
             #expression(paste(bar(Y)[B],"    ")),
             expression(paste(bar(Y)[A(adj)])),
             expression(paste(bar(Y)[B(adj)]))),
           las=2)
      text(upperLeft(offset=0.05),"h)", cex=2)
      legend("topright",legend=c("Group A", "Group B"), pch=c(16,21),pt.bg=c("black","white"),col=1,bty="n")
      
      box(bty="o")

    }
    par(old)
  


###################################################
### chunk number 3: AncovaKey1 eval=FALSE
###################################################
## boxplot(DV~Factor, dataset)


###################################################
### chunk number 4: AncovaKey2 eval=FALSE
###################################################
## plot(aov(DV~CV+Factor, dataset), which=1)


###################################################
### chunk number 5: AncovaHomogeneityTest eval=FALSE
###################################################
## library(lattice)
## xyplot(DV~CV|FACTOR, dataset, type=c("r","p"))
## #OR
## library(car)
## scatterplot(DV~CV|FACTOR, dataset)
## #inference test for interaction (non-homogenous slopes)
## anova(aov(DV~CV*FACTOR, dataset))


###################################################
### chunk number 6: AncovaAncova eval=FALSE
###################################################
## data.aov <- aov(DV~CV+FACTOR, dataset)
## anova(data.aov)


###################################################
### chunk number 7: AncovaLM eval=FALSE
###################################################
## summary(lm(DV ~ CV, dataset, subset=FACTOR=="A"))


###################################################
### chunk number 8: AncovaCut eval=FALSE
###################################################
## dataset$CV_F <- cut(dataset$CV, 4)
## data.aov1<-aov(DV~CV_F*FACTOR, data=dataset)


###################################################
### chunk number 9: AncovaSplit eval=FALSE
###################################################
## CV_sd2<-mean(CV)-2*sd(CV)
## data.aov1<-aov(DV~FACTOR+c(CV-CV_sd2), data=dataset)
## anova(data.lm2)


###################################################
### chunk number 10: AncovaJN eval=FALSE
###################################################
## data.lm<-lm(DV~CV*FACTOR, dataset)
## library(biology)
## wilcox.JN(data.lm, type="H")


###################################################
### chunk number 11: PartridgeReadTable
###################################################
partridge <- read.table('partridge.csv', header=T, sep=',')


###################################################
### chunk number 12: Options
###################################################
op <- options(width=40)


###################################################
### chunk number 13: partridgeResid1
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(aov(LONGEV~THORAX + 
TREATMENT, partridge), which=1)


###################################################
### chunk number 14: partridgeResid2
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(aov(log10(LONGEV) ~ THORAX + 
TREATMENT, partridge), which=1)


###################################################
### chunk number 15: Options
###################################################
options(op)


###################################################
### chunk number 16: partridgeLattice1
###################################################
getOption("SweaveHooks")[["fig"]]()
library(lattice)
print(xyplot(log10(LONGEV)~THORAX|TREATMENT, partridge, type=c("r","p")))


###################################################
### chunk number 17: PartridgeAnova
###################################################
anova(aov(log10(LONGEV) ~ THORAX*TREATMENT, partridge))


###################################################
### chunk number 18: Options
###################################################
op <- options(width=50)


###################################################
### chunk number 19: partridgeLattice2
###################################################
getOption("SweaveHooks")[["fig"]]()
library(lattice)
print(with(partridge,xyplot(log10(LONGEV) ~ THORAX, groups=TREATMENT, 
type=c("p","r"), col=1, par.settings = list(superpose.symbol 
= list(pch=1:5,col=1), superpose.line=list(lty=1:6)), 
key=list(space ="right", lty=1:5, lines=T, points=T, pch=1:5,col=1, 
text=list(levels(TREATMENT))))))


###################################################
### chunk number 20: Options
###################################################
options(op)


###################################################
### chunk number 21: PartridgeAnova1
###################################################
anova(aov(THORAX ~TREATMENT, partridge))


###################################################
### chunk number 22: PartridgeContrasts
###################################################
# define contrasts
contrasts(partridge$TREATMENT) <- cbind(c(0, 0.5, 0.5, -0.5, -0.5), 
c(0, 0, 0, 1, -1))
# confirm that contrasts orthogonal
round(crossprod(contrasts(partridge$TREATMENT)), 1)


###################################################
### chunk number 23: Options
###################################################
op <- options(width=50)


###################################################
### chunk number 24: PartridgeAncova
###################################################
partridge.aov <- aov(log10(LONGEV) ~ THORAX+TREATMENT, partridge)
library(biology)
AnovaM(partridge.aov, type="III", split=list(TREATMENT=
list("Preg vs Virg"=1, "1 Virg vs 8 Virg"=2)))


###################################################
### chunk number 25: Options
###################################################
options(op)


###################################################
### chunk number 26: partridgePlot1
###################################################
getOption("SweaveHooks")[["fig"]]()
# create the base blank plot
plot(LONGEV ~ THORAX, partridge, type = "n", axes = F, xlab = "",
ylab = "", log = "y")
xs <- seq(min(partridge$THORAX), max(partridge$THORAX), l = 1000)
# plot the None series
part.lm <- lm(LONGEV ~ THORAX, partridge, subset = TREATMENT == "None")
lines(xs, predict(part.lm, data.frame(THORAX = xs)), lty = 1)
points(LONGEV ~ THORAX, partridge, subset = TREATMENT == "None", 
type = "p", pch = 1)
# plot the Preg1 series
part.lm <- lm(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Preg1")
lines(xs, predict(part.lm, data.frame(THORAX = xs)), lty = 2)
points(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Preg1", 
type = "p", pch = 23, bg = "gray")
# plot the Preg8 series
part.lm <- lm(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Preg8")
lines(xs, predict(part.lm, data.frame(THORAX = xs)), lty = 3)
points(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Preg8", 
type = "p", pch = 24, bg = "gray")
# plot the Virg1 series
part.lm <- lm(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Virg1")
lines(xs, predict(part.lm, data.frame(THORAX = xs)), lty = 4)
points(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Virg1", 
type = "p", pch = 23, bg = "black")
# plot the Virg8 series
part.lm <- lm(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Virg8")
lines(xs, predict(part.lm, data.frame(THORAX = xs)), lty = 5)
points(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Virg8", 
type = "p", pch = 24, bg = "black")
axis(1)
mtext("Thorax length (mm)", 1, line = 3)
axis(2, las = 1)
mtext(expression(paste("Male fruitfly longevity (days)")), 2, line = 3)
legend("bottomright", legend = c("None", "1 pregnant", "8 pregnant", "1virgin", 
"8 virgin"), bty = "n", title = "Treatment", lty = 1:6, pch = c(1, 23, 24, 23, 24), 
pt.bg = c(1, "gray", "gray", 1, 1))
box(bty = "l")


###################################################
### chunk number 27: ConstableReadTable
###################################################
constable <- read.table('constable.csv', header=T, sep=',')


###################################################
### chunk number 28: constablePlot1
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
scatterplot(SUTW ~ IV|TREAT, constable)


###################################################
### chunk number 29: Options
###################################################
op <- options(width=40)


###################################################
### chunk number 30: constablePlot2
###################################################
getOption("SweaveHooks")[["fig"]]()
library(car)
scatterplot(SUTW ~ I(IV^(1/3))|TREAT, constable)


###################################################
### chunk number 31: Options
###################################################
op <- options(width=65)


###################################################
### chunk number 32: constableLattice1
###################################################
getOption("SweaveHooks")[["fig"]]()
library(lattice)
print(with(constable,xyplot(SUTW~I(IV^(1/3)),groups=TREAT, type=c("p","r"), col=1,
par.settings = list(superpose.symbol = list(pch=1:3,col=1),
superpose.line=list(lty=1:3)),
key=list(space="right", lty=1:3, lines=T, points=T, pch=1:3, col=1, text=list(levels(TREAT)))
)
))


###################################################
### chunk number 33: ConstableAOV
###################################################
anova(aov(SUTW ~ I(IV^(1/3))*TREAT, constable))


###################################################
### chunk number 34: ConstableWilcoxJN
###################################################
library(biology)
#fit model using a lm model (since must be able to extract intercept and slope)
constable.lm<-lm(SUTW~I(IV^(1/3))*TREAT, constable)
wilcox.JN(constable.lm, type="H")


###################################################
### chunk number 35: constableScatterPlot2
###################################################
getOption("SweaveHooks")[["fig"]]()
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
legend.vfont("topleft", c("\\#H0841 Initial","\\#H0844 High","\\#H0852 Low"), bty="n", 
lty=c(1,2,3), merge=F, title="Food regime", vfont=c("serif","plain"))
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


###################################################
### chunk number 36: CreateRFile
###################################################
Stangle("../../ancova.rnw")


