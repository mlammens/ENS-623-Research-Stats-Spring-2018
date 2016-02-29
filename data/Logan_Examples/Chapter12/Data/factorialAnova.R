###################################################
### chunk number 1: FactorialAnova_SetDir
###################################################
setwd("FactorialAnova/Data")
setCacheDir("../cache")


###################################################
### chunk number 2: factorialDiagram
###################################################
library(sp)
circle <- function(x, y, radius, units=c("cm", "in"), segments=100){
  units <- match.arg(units) 
  if (units == "cm") radius <- radius/2.54 
  plot.size <- par("pin") 
  plot.units <- par("usr") 
  units.x <- plot.units[2] - plot.units[1] 
  units.y <- plot.units[4] - plot.units[3] 
  ratio <- (units.x/plot.size[1])/(units.y/plot.size[2]) 
  size <- radius*units.x/plot.size[1] 
  angles <- (0:segments)*2*pi/segments 
  unit.circle <- cbind(cos(angles), sin(angles)) 
  shape <- matrix(c(1, 0, 0, 1/(ratio^2)), 2, 2) 
  ellipse <- t(c(x, y) + size*t(unit.circle %*% chol(shape))) 
  lines(ellipse)
  ellipse
} 

op<- par(mar=c(0,0,1,0))
plot(c(0,1),c(0,1),xlim=c(0,100), ylim=c(0,100), xlab="", ylab="",type="n", axes=F)
sx<-seq(5,100-15,l=6)
sy<-seq(5,100-45,l=2)
dens<-c(20,20,20,20,20,20,0,0,0,0,0,0)
shape <-gl(2,1,12,lab=c("r","c")) 
ll<-0
#xl<-vector("numeric",length(sx)*length(sy))
#yl<-vector("numeric",length(sx)*length(sy))
bx<-1:12
bx<-sample(bx,replace=F)
bx_i <- 0
for (i in 1:length(sx)) {
  for (j in 1:length(sy)){
    ll<-ll+1
    rect(sx[i], sy[j],sx[i]+15, sy[j]+45, density=0,col="gray")   
    fin<-T
    while (fin) {
                                        #for (kk in 1:5){
      xx<-runif(1,sx[i],sx[i]+15)
      yy<-runif(1,sy[j],sy[j]+45)
      if (all(point.in.polygon(c(xx,xx+10)
                               ,c(yy,yy+10)
                               ,c(sx[i],sx[i]+15,sx[i]+15,sx[i],sx[i])
                               ,c(sy[j],sy[j],sy[j]+45,sy[j]+45,sy[j]))>0)) {
        bx_i<-bx_i+1
        if(shape[bx[bx_i]]=="r") {
          rect(xx, yy, xx+10, yy+10, density=dens[bx[bx_i]],angle=45,col="grey")
          rect(xx, yy, xx+10, yy+10, density=0,angle=45)
        }
        else {
          polygon(circle(xx+5,yy+5, 5/7, seg=100),density=dens[bx[bx_i]],angle=45, col="grey")
          polygon(circle(xx+5,yy+5, 5/7, seg=100), density=0,col="black")
        }
        fin<-F
      }
    }   
  }
}         
text(5,102,"Factor A  -", font=2, pos=4)
rect(25, 101,25+3, 101+3, density=20, col="gray")
                                        #rect(30, 101,78+3, 101+3)
text(31,102," or ", font=2)
rect(34, 101,34+3, 101+3, col="gray",density=0)


text(40,102,"Factor B  -", font=2, pos=4)
rect(60, 101,60+3, 101+3,density=0)
                                        #rect(30, 101,78+3, 101+3)
text(66,102," or ", font=2)
polygon(circle(71,102.5, .2, seg=100), density=0,col="black")
rect(34, 101,34+3, 101+3, col="gray",density=0)
par(op)


###################################################
### chunk number 3: factorialInteractionPlots1
###################################################
source("../../Figure_helper.R")
plotIt <- function(response, x, g,yrange=NULL){
  means <- tapply(response, list(x, g), mean)
  library(gmodels)
  error <- tapply(response, list(x, g), function(x) ci(x)[[4]])
  if (is.null(yrange)) yrange <- c(min(means - error), max(means + error))
  n.levels_x<-length(levels(x))
  n.levels_grp<-length(levels(g))
  plot(c(1, n.levels_x), yrange,xlim=c(.75,2.25),type = "n", xlab = "", 
            ylab = "", axes = FALSE, main = NULL)
  box(bty="l")
  axis(1,at=c(1,2),lab=c("High","Low"), font=2)
  mtext("Response (mean growth rate of seedlings)",2,line=2,font=2,cex=1)
  mtext("Factor A (temperature)",1,line=3,font=2,cex=1)
  col <- c(1,1)
  lty=c(1,2)
  for (i in 1:n.levels_grp) {
    points(1:n.levels_x, means[, i], type = "b", pch = 16, cex = 1.25, col = col[i], lty = lty[i])
    arrows(1:n.levels_x, means[, i] - error[, i], 1:n.levels_x, means[, i] + error[, i], angle = 90, code = 3, col = col[i], lty = 1, length = 0.01)
  }
}
op<- par(mfrow=c(1,4),mar=c(5,5,0.1,0.1),xpd=NA)
  temp <- gl(2,6,12,lab=c("High","Low"))
  fert <- gl(2,3,12,lab=c("Fert","Control"))
  #growth <- c(rnorm(3,9,.5),rnorm(3,7,.5),rnorm(3,6,.5),rnorm(3,6,.5))
  growth <-c(8.9648360872015, 9.30179930312554, 9.30259981308179, 7.16360961823637, 7.45517592411305, 6.92648560758747, 5.2904433517243, 6.4748196474218, 6.19303502608774, 6.17336989304315, 6.26051423369468, 5.86310459058649)
  plants <- data.frame(temp,fert,growth)
  plotIt(plants$growth, plants$temp, plants$fert, yrange=c(5,10))
  text(upperLeft(), "a)", font=2, cex=1.5)

  temp <- gl(2,6,12,lab=c("High","Low"))
  fert <- gl(2,3,12,lab=c("Fert","Control"))
  #growth1 <- c(rnorm(3,9,.5),rnorm(3,7,.5),rnorm(3,7,.5),rnorm(3,6,.5))
  growth1 <-c(9.3209425145248, 8.56010973427994, 8.9461687613582, 7.0125506256794, 7.39409785103783, 6.533772692321, 7.58194383678464, 7.43026530519029, 7.35025326593211, 6.26191982885808, 6.27347996903821, 5.70867164606299)
  plants <- data.frame(temp,fert,growth1)
  plotIt(plants$growth1, plants$temp, plants$fert, yrange=c(5,10))
  text(upperLeft(), "b)", font=2, cex=1.5)

  temp <- gl(2,6,12,lab=c("High","Low"))
  fert <- gl(2,3,12,lab=c("Fert","Control"))
  #growth2 <- c(rnorm(3,8,.5),rnorm(3,7,.5),rnorm(3,8,.5),rnorm(3,7,.5))
  growth2 <-c(8.38041959484204, 7.47977832460672, 7.47176845716182, 7.43783366611174, 6.59148577941757, 6.32534079262335, 7.67983501174738, 8.28698024791157, 7.28276980666923, 6.36610825094184, 6.89575946115637, 7.27237117641956)
  plants <- data.frame(temp,fert,growth2)
  plotIt(plants$growth2, plants$temp, plants$fert, yrange=c(5,10))
  text(upperLeft(), "c)", font=2, cex=1.5)

  temp <- gl(2,6,12,lab=c("High","Low"))
  fert <- gl(2,3,12,lab=c("Fert","Control"))
  #growth3 <- c(rnorm(3,6,.5),rnorm(3,8,.5),rnorm(3,8,.5),rnorm(3,6,.5))
  growth3 <-c(5.79563575369473, 5.59597547857384, 6.42491014977645, 7.78344533489109, 7.70044043693498, 7.76929149313954, 8.10639582677468, 7.9484750480462,7.86786567576178, 6.69471028758982, 5.78707257668315, 5.31222456410047)
  plants <- data.frame(temp,fert,growth3)
  plotIt(plants$growth3, plants$temp, plants$fert, yrange=c(5,10))
  text(upperLeft(), "d)", font=2, cex=1.5)
par(op)


###################################################
### chunk number 4: SSLibrary
###################################################
library(PBSmapping)
circle <- function(x, y, radius, units=c("cm", "in"), segments=100){
       units <- match.arg(units) 
     if (units == "cm") radius <- radius/2.54 
     plot.size <- par("pin") 
     plot.units <- par("usr") 
     units.x <- plot.units[2] - plot.units[1] 
     units.y <- plot.units[4] - plot.units[3] 
     ratio <- (units.x/plot.size[1])/(units.y/plot.size[2]) 
     size <- radius*units.x/plot.size[1] 
     angles <- (0:segments)*2*pi/segments 
     unit.circle <- cbind(cos(angles), sin(angles)) 
     shape <- matrix(c(1, 0, 0, 1/(ratio^2)), 2, 2) 
     ellipse <- t(c(x, y) + size*t(unit.circle %*% chol(shape))) 
     #lines(ellipse)
       ellipse
     } 


###################################################
### chunk number 5: SSS1
###################################################
postscript(file="../Figures/fig-SS1.eps", width=6, height=9,paper='special', horizontal=F)
op <- par(mfrow=c(3,2), mar=c(0,0,0,0),oma=c(0,0,0,0), xpd=T)
##start with balanced design
circ1<-circle(-2,2,20, seg=100)
circ2<-circle(2,2,20)
circ3<-circle(0,-2,20)
circ1a<-circle(-2.65,2.65,20, seg=100)
circ2a<-circle(2.65,2.65,20)
circ3a<-circle(0,-1.95,20)
polysR<-data.frame(PID=rep(1, 5), POS=1:5, X=c(-5.5,5.5,5.5,-5.5,-5.5), Y=c(-5.5,-5.5,5.5,5.5,-5.5))
polysA <- data.frame(PID=rep(1, 101), POS=1:101, X=circ1a[,1], Y=circ1a[,2])
polysB <- data.frame(PID=rep(1, 101), POS=1:101, X=circ2a[,1], Y=circ2a[,2])
polysA_B<-joinPolys(polysA,polysB, operation="UNION")
polysAB <- data.frame(PID=rep(1, 101), POS=1:101, X=circ3a[,1], Y=circ3a[,2])

#Type I
plotMap(polysA, xlim=c(-7,7), ylim=c(-5.5,5.5), projection=1,axes=F,xlab="",ylab="", type="n",plt=c(0,1,0,1))
addPolys(polysR, col="gray", density=20)
addPolys(polysA, col="white", border="white")
addPolys(polysB, col="white", border="white")
addPolys(polysAB, col="white", border="white")
addPolys(polysA, col="grey90")
addPolys(joinPolys(polysB, polysA, operation="DIFF"), col="grey80")
addPolys(joinPolys(polysAB, polysA_B, operation="DIFF"), col="black")

text(-2.5,2.5, "Factor A", font=2, cex=1)
text(2.5,2.5, "Factor B", font=2, cex=1)
text(0,-2, "A:B\nInteraction", col="white",font=2, cex=1)
text(-5.5,-5, "Residuals", font=2, cex=1, pos=4)
text(-5.5,6,"a) Balanced factorial design", font=2, cex=1.5, pos=4)

lines(c(-7,7),c(-7,-7), lwd=2)
lines(c(7,7),c(-7,7), lwd=2)
lines(c(-7,7),c(7,7), lwd=2)
lines(c(-7,-7),c(-7,7), lwd=2)

#Unbalanced
polysR<-data.frame(PID=rep(1, 5), POS=1:5, X=c(-5.5,5.5,5.5,-5.5,-5.5), Y=c(-5.5,-5.5,5.5,5.5,-5.5))
polysA <- data.frame(PID=rep(1, 101), POS=1:101, X=circ1[,1], Y=circ1[,2])
polysB <- data.frame(PID=rep(1, 101), POS=1:101, X=circ2[,1], Y=circ2[,2])
polysA_B<-joinPolys(polysA,polysB, operation="UNION")
polysAB <- data.frame(PID=rep(1, 101), POS=1:101, X=circ3[,1], Y=circ3[,2])
plotMap(polysA, xlim=c(-7,7), ylim=c(-5.5,5.5), projection=1,axes=F,xlab="",ylab="", type="n",plt=c(0,1,0,1))
addPolys(polysR, col="white", density=20)
addPolys(polysA, border="black")
addPolys(polysB, border="black")
addPolys(polysAB, border="black")
text(-2,2, "Factor A", font=2, cex=1)
text(2,2, "Factor B", font=2, cex=1)
text(0,-2, "A:B\nInteraction", col="black",font=2, cex=1)
text(-5.5,-5, "Residuals", font=2, cex=1, pos=4)
text(-5.5,6,"b) Positive intersection", font=2, cex=1.5, pos=4)

lines(c(-7,7),c(7,7), lwd=2)
lines(c(7,7),c(-7,7), lwd=2)
lines(c(-7,-7),c(-7,7), lwd=2)
#Now start the unbalanced bits


polysR<-data.frame(PID=rep(1, 5), POS=1:5, X=c(-5.5,5.5,5.5,-5.5,-5.5), Y=c(-5.5,-5.5,5.5,5.5,-5.5))
polysA <- data.frame(PID=rep(1, 101), POS=1:101, X=circ1[,1], Y=circ1[,2])
polysB <- data.frame(PID=rep(1, 101), POS=1:101, X=circ2[,1], Y=circ2[,2])
polysA_B<-joinPolys(polysA,polysB, operation="UNION")
polysAB <- data.frame(PID=rep(1, 101), POS=1:101, X=circ3[,1], Y=circ3[,2])

#Type I - A first
plotMap(polysA, xlim=c(-7,7), ylim=c(-5.5,5.5), projection=1,axes=F,xlab="",ylab="", type="n",plt=c(0,1,0,1))
addPolys(polysR, col="gray", density=20)

addPolys(polysA, col="white", border="white")
addPolys(polysB, col="white", border="white")
addPolys(polysAB, col="white", border="white")

addPolys(polysA, col="grey90")
addPolys(joinPolys(polysB, polysA, operation="DIFF"), col="grey80")
addPolys(joinPolys(polysAB, polysA_B, operation="DIFF"), col="black")

text(-2,2, "Factor A", font=2, cex=1)
text(2.75,2, "Factor B", font=2, cex=1)
text(0,-2, "A:B\nInteraction", col="white",font=2, cex=1)
text(-5.5,-5, "Residuals", font=2, cex=1, pos=4)
text(-5.5,6,"c) Type I SS - Factor A first", font=2, cex=1.5, pos=4)

lines(c(-7,7),c(7,7), lwd=2)
lines(c(-7,-7),c(-7,7), lwd=2)

#Type I - B first
plotMap(polysA, xlim=c(-7,7), ylim=c(-5.5,5.5), projection=1,axes=F,xlab="",ylab="", type="n",plt=c(0,1,0,1))
addPolys(polysR, col="gray", density=20)

addPolys(polysA, col="white", border="white")
addPolys(polysB, col="white", border="white")
addPolys(polysAB, col="white", border="white")

addPolys(polysB, col="grey80")
addPolys(joinPolys(polysA, polysB, operation="DIFF"), col="grey90")
addPolys(joinPolys(polysAB, polysA_B, operation="DIFF"), col="black")

text(-2.75,2, "Factor A", font=2, cex=1)
text(2,2, "Factor B", font=2, cex=1)
text(0,-2, "A:B\nInteraction", col="white",font=2, cex=1)
text(-5.5,-5, "Residuals", font=2, cex=1, pos=4)
text(-5.5,6,"d) Type I SS - Factor B first", font=2, cex=1.5, pos=4)

lines(c(7,7),c(-7,7), lwd=2)

#Type II
plotMap(polysA, xlim=c(-7,7), ylim=c(-5.5,5.5), projection=1,axes=F,xlab="",ylab="", type="n",plt=c(0,1,0,1))
addPolys(polysR, col="gray", density=20)
addPolys(polysA, col="white", border="white")
addPolys(polysB, col="white", border="white")
addPolys(polysAB, col="white", border="white")

addPolys(joinPolys(polysA,polysB, operation="DIFF"), col="grey80")
addPolys(joinPolys(polysB,polysA, operation="DIFF"), col="grey90")
addPolys(joinPolys(polysAB, polysA_B, operation="DIFF"), col="black")
#addPolys(joinPolys(polysA,polysB, operation="INT"), col="gray", density=20)
text(-2.75,2, "Factor A", font=2, cex=1)
text(2.75,2, "Factor B", font=2, cex=1)
text(0,-2, "A:B\nInteraction", col="white",font=2, cex=1)
text(-5.5,-5, "Residuals", font=2, cex=1, pos=4)
text(-5.5,6,"e) Type II SS", font=2, cex=1.5, pos=4)

lines(c(-7,7),c(-7,-7), lwd=2)
lines(c(-7,-7),c(-7,7), lwd=2)

#Type III
plotMap(polysA, xlim=c(-7,7), ylim=c(-5.5,5.5), projection=1,axes=F,xlab="",ylab="", type="n",plt=c(0,1,0,1))
addPolys(polysR, col="gray", density=20)
addPolys(polysA, col="white", border="white")
addPolys(polysB, col="white", border="white")
addPolys(polysAB, col="white", border="white")

polysA1<-joinPolys(polysA,polysB, operation="DIFF")
polysA2<-joinPolys(polysA1,polysAB, operation="DIFF")
addPolys(polysA2, col="grey90")

polysB1<-joinPolys(polysB,polysA, operation="DIFF")
polysB2<-joinPolys(polysB1,polysAB, operation="DIFF")
addPolys(polysB2, col="grey80")
addPolys(joinPolys(polysAB, polysA_B, operation="DIFF"), col="black")

text(-2.75,2, "Factor A", font=2, cex=1)
text(2.75,2, "Factor B", font=2, cex=1)
text(0,-2, "A:B\nInteraction", col="white",font=2, cex=1)
text(-5.5,-5, "Residuals", font=2, cex=1, pos=4)
text(-5.5,6,"f) Type III SS", font=2, cex=1.5, pos=4)

lines(c(-7,7),c(-7,-7), lwd=2)
lines(c(7,7),c(-7,7), lwd=2)
par(op)
dev.off()


###################################################
### chunk number 6: SSS2
###################################################
postscript(file="../Figures/fig-SS2.eps", width=6, height=9,paper='special', horizontal=F)
op <- par(mfrow=c(3,2), mar=c(0,0,0,0),oma=c(0,0,0,0), xpd=T)
circ1<-circle(-2.75,1.5,18, seg=100)
circ2<-circle(2.75,1.5,18)
circ3<-circle(0,-1.5,18)
polysR<-data.frame(PID=rep(1, 5), POS=1:5, X=c(-5.5,5.5,5.5,-5.5,-5.5), Y=c(-5.5,-5.5,5.5,5.5,-5.5))
polysA <- data.frame(PID=rep(1, 101), POS=1:101, X=circ1[,1], Y=circ1[,2])
polysB <- data.frame(PID=rep(1, 101), POS=1:101, X=circ2[,1], Y=circ2[,2])
polysA_B<-joinPolys(polysA,polysB, operation="UNION")
polysAB <- data.frame(PID=rep(1, 101), POS=1:101, X=circ3[,1], Y=circ3[,2])
joinA_B<-data.frame(PID=rep(1, 5), POS=1:5, X=c(-3,3,3,-3,-3), Y=c(.5,.5,2.5,2.5,.5))

plotMap(polysA, xlim=c(-7,7), ylim=c(-5.5,5.5), projection=1,axes=F,xlab="",ylab="", type="n",plt=c(0,1,0,1))
addPolys(polysR)
addPolys(polysA)
addPolys(polysB)
addPolys(polysAB)
#addPolys(joinPolys(joinPolys(joinA_B,polysA,operation="DIFF"),polysB, operation="DIFF"))
text(-2.75,1.5, "Factor A", font=2, cex=1)
text(2.75,1.5, "Factor B", font=2, cex=1)
text(0,-1.5, "A:B\nInteraction",font=2, cex=1)
text(-5.5,-5, "Residuals", font=2, cex=1, pos=4)
text(-5.5,6,"g) Negative intersection", font=2, cex=1.5, pos=4)

lines(c(-7,7),c(7,7), lwd=2)
lines(c(-7,-7),c(-7,7), lwd=2)

arrows(6,0,7,0, length=.09)

plotMap(polysA, xlim=c(-7,7), ylim=c(-5.5,5.5), projection=1,axes=F,xlab="",ylab="", type="n",plt=c(0,1,0,1))
addPolys(polysR)
addPolys(polysA)
addPolys(polysB)
addPolys(polysAB)
addPolys(joinPolys(joinPolys(joinA_B,polysA,operation="DIFF"),polysB, operation="DIFF"))
text(-2.75,1.5, "Factor A", font=2, cex=1)
text(2.75,1.5, "Factor B", font=2, cex=1)
text(0,-1.5, "A:B\nInteraction",font=2, cex=1)
text(-5.5,-5, "Residuals", font=2, cex=1, pos=4)
text(-5.5,6,"h) Negative intersection", font=2, cex=1.5, pos=4)

lines(c(-7,7),c(7,7), lwd=2)
lines(c(7,7),c(-7,7), lwd=2)

#Type I - A first
plotMap(polysA, xlim=c(-7,7), ylim=c(-5.5,5.5), projection=1,axes=F,xlab="",ylab="", type="n",plt=c(0,1,0,1))
addPolys(polysR, col="gray", density=20)
addPolys(polysA, col="white", border="white")
addPolys(polysB, col="white", border="white")
addPolys(polysAB, col="white", border="white")
#addPolys(joinPolys(joinPolys(joinA_B,polysA,operation="DIFF"),polysB, operations="DIFF"))

addPolys(polysA, col="grey80")
polysBuA<-joinPolys(polysB,joinA_B, operation="UNION")
polysBuA<-joinPolys(polysBuA,polysA, operation="DIFF")
addPolys(polysBuA, col="grey90")
polysA_B<-joinPolys(polysA,polysBuA, operation="UNION")
addPolys(joinPolys(polysAB, polysA_B, operation="DIFF"), col="black")
#addPolys(joinPolys(polysA,polysB, operation="INT"), col="gray", density=20)
text(-2.75,1.5, "Factor A", font=2, cex=1)
text(2.75,1.5, "Factor B", font=2, cex=1)
text(0,-1.5, "A:B\nInteraction", col="white",font=2, cex=1)
text(-5.5,-5, "Residuals", font=2, cex=1, pos=4)
text(-5.5,6,"i) Type I SS - Factor A first", font=2, cex=1.5, pos=4)

lines(c(-7,-7),c(-7,7), lwd=2)

#Type I - B first
plotMap(polysA, xlim=c(-7,7), ylim=c(-5.5,5.5), projection=1,axes=F,xlab="",ylab="", type="n",plt=c(0,1,0,1))
addPolys(polysR, col="gray", density=20)
addPolys(polysA, col="white", border="white")
addPolys(polysB, col="white", border="white")
addPolys(polysAB, col="white", border="white")
#addPolys(joinPolys(joinPolys(joinA_B,polysA,operation="DIFF"),polysB, operations="DIFF"))
addPolys(polysB, col="grey80")
polysBuA<-joinPolys(polysA,joinA_B, operation="UNION")
polysBuA<-joinPolys(polysBuA,polysB, operation="DIFF")
addPolys(polysBuA, col="grey90")
polysA_B<-joinPolys(polysB,polysBuA, operation="UNION")
addPolys(joinPolys(polysAB, polysA_B, operation="DIFF"), col="black")
#addPolys(joinPolys(polysA,polysB, operation="INT"), col="gray", density=20)
text(-2.75,1.5, "Factor A", font=2, cex=1)
text(2.75,1.5, "Factor B", font=2, cex=1)
text(0,-1.5, "A:B\nInteraction", col="white",font=2, cex=1)
text(-5.5,-5, "Residuals", font=2, cex=1, pos=4)
text(-5.5,6,"j) Type I SS - Factor B first", font=2, cex=1.5, pos=4)
lines(c(7,7),c(-7,7), lwd=2)
#Type II
plotMap(polysA, xlim=c(-7,7), ylim=c(-5.5,5.5), projection=1,axes=F,xlab="",ylab="", type="n",plt=c(0,1,0,1))
addPolys(polysR, col="gray", density=20)
addPolys(polysA, col="white", border="white")
addPolys(polysB, col="white", border="white")
addPolys(polysAB, col="white", border="white")
addPolys(joinA_B, col="white", border="white")

polysBuA<-joinPolys(polysB,joinA_B, operation="UNION")
polysBuA<-joinPolys(polysA,polysBuA, operation="UNION")
addPolys(polysBuA, col="grey90", border="black", lwd=2)
addPolys(joinPolys(polysAB, polysA_B, operation="DIFF"), col="black")

polysBuA<-joinPolys(joinA_B,polysB, operation="DIFF")
polysBuA<-joinPolys(polysBuA,polysA, operation="DIFF")
addPolys(polysBuA, col="grey70", border=0)

addPolys(polysA, col="grey80", border=0)
addPolys(polysB, col="grey90", border=0)

text(-2.75,2, "Factor A", font=2, cex=1)
text(2.75,2, "Factor B", font=2, cex=1)
text(0,-2, "A:B\nInteraction", col="white",font=2, cex=1)
text(-5.5,-5, "Residuals", font=2, cex=1, pos=4)
text(-5.5,6,"k) Type II SS", font=2, cex=1.5, pos=4)

lines(c(-7,7),c(-7,-7), lwd=2)
lines(c(-7,-7),c(-7,7), lwd=2)

#Type III
plotMap(polysA, xlim=c(-7,7), ylim=c(-5.5,5.5), projection=1,axes=F,xlab="",ylab="", type="n",plt=c(0,1,0,1))
addPolys(polysR, col="gray", density=20)
addPolys(polysA, col="white", border="white")
addPolys(polysB, col="white", border="white")
addPolys(polysAB, col="white", border="white")
addPolys(joinA_B, col="white", border="white")

polysBuA<-joinPolys(polysB,joinA_B, operation="UNION")
polysBuA<-joinPolys(polysA,polysBuA, operation="UNION")
polysBuA<-joinPolys(polysBuA, polysAB,operation="DIFF")
addPolys(polysBuA, col="grey90", border="black", lwd=2)
addPolys(joinPolys(polysAB, polysA_B, operation="DIFF"), col="black")

polysBuA<-joinPolys(joinA_B,polysB, operation="DIFF")
polysBuA<-joinPolys(polysBuA,polysA, operation="DIFF")
addPolys(polysBuA, col="grey70", border=0)

addPolys(joinPolys(polysA,polysAB, operation="DIFF"), col="grey80", border=0)
addPolys(joinPolys(polysB,polysAB, operation="DIFF"), col="grey90", border=0)

text(-2.75,2, "Factor A", font=2, cex=1)
text(2.75,2, "Factor B", font=2, cex=1)
text(0,-2, "A:B\nInteraction", col="white",font=2, cex=1)
text(-5.5,-5, "Residuals", font=2, cex=1, pos=4)
text(-5.5,6,"l) Type III SS", font=2, cex=1.5, pos=4)

lines(c(-7,7),c(-7,-7), lwd=2)
lines(c(7,7),c(-7,7), lwd=2)
par(op)
dev.off()


###################################################
### chunk number 7: FAKey1 eval=FALSE
###################################################
## boxplot(DV~A, data) #factor A
## boxplot(DV~B, data) #factor B
## boxplot(DV~A*B, data) #A:B interaction


###################################################
### chunk number 8: FAKey2 eval=FALSE
###################################################
## library(nlme)
## data.AB.agg <- gsummary(data,groups=data$A:data$B)
## boxplot(DV~A, data.AB.agg) #factor A


###################################################
### chunk number 9: FAKey3 eval=FALSE
###################################################
## contrasts(data$A)<-cbind(c(contrasts),...)
## round(crossprod(contrasts(data$A)), 2)


###################################################
### chunk number 10: FAKey4 eval=FALSE
###################################################
## replications(DV~A*b*C+.., data)
## library(biology)
## is.balanced(DV~A*b*C+.., data)


###################################################
### chunk number 11: FAKey5 eval=FALSE
###################################################
## data.aov<-aov(DV~A*B, data)


###################################################
### chunk number 12: FAKey6 eval=FALSE
###################################################
## library(biology)
## AnovaM(data.aov, split=list(A=list("Name1"=1, "Name2"=2, ...), B=list()))
## #OR
## summary(data.aov, split=list(A=list("Name1"=1, "Name2"=2, ...), B=list()))


###################################################
### chunk number 13: FAKey7 eval=FALSE
###################################################
## AnovaM(data.aov)
## #OR
## summary(data.aov)
## #OR
## anova(data.aov)


###################################################
### chunk number 14: FAKey8 eval=FALSE
###################################################
## data.aov<-aov(DV~A*B, data)


###################################################
### chunk number 15: TEMP
###################################################
op <-options(width=70)


###################################################
### chunk number 16: FAKey9 eval=FALSE
###################################################
## AnovaM(data.aov, denoms=c("A:B","Resid","Resid"), split=list(A= list("Name1"=1, "Name2"=2, ...), B=list()))


###################################################
### chunk number 17: TEMP1
###################################################
options(op)


###################################################
### chunk number 18: FAKey10 eval=FALSE
###################################################
## AnovaM(data.aov, denoms=c("A:B","Resid","Resid"))


###################################################
### chunk number 19: FAKey11 eval=FALSE
###################################################
## data.aov<-aov(DV~A*B, data)


###################################################
### chunk number 20: FAKey12 eval=FALSE
###################################################
## AnovaM(data.aov, type="III", split=list(A=list("Name1"=1, "Name2"=2, ...), 
## B=list()))


###################################################
### chunk number 21: FAKey13 eval=FALSE
###################################################
## contrasts(data$A)<-contr.helmert
## contrasts(data$B)<-contr.helmert
## data.aov<-aov(DV~A*B, data)
## AnovaM(data.aov, type="III", data)


###################################################
### chunk number 22: FAKey14 eval=FALSE
###################################################
## data.aov<-aov(DV~A*B, data)


###################################################
### chunk number 23: FAKey15 eval=FALSE
###################################################
## AnovaM(data.aov, denoms=c("A:B","Resid","Resid"), type="III", split=list(
## A=list("Name1"=1, "Name2"=2, ...), B=list()))


###################################################
### chunk number 24: FAKey16 eval=FALSE
###################################################
## AnovaM(data.aov, denoms=c("A:B","Resid","Resid"), type="III")


###################################################
### chunk number 25: FAKey17 eval=FALSE
###################################################
## data$AB<-factor(paste(data$A, data$B, sep="A:B"))
## contrasts(data$AB)<-cbind(c(contrasts), c(contrasts),...)


###################################################
### chunk number 26: FAKey17 eval=FALSE
###################################################
## replications(DV~A*b*C+.., data)
## library(biology)
## is.balanced(DV~A*b*C+.., data)


###################################################
### chunk number 27: FAKey18 eval=FALSE
###################################################
## data.aov<-aov(DV~AB, data)


###################################################
### chunk number 28: FAKey19 eval=FALSE
###################################################
## AnovaM(data.aov, split=list(AB=list("Factor A"=1:2)))


###################################################
### chunk number 29: FAKey20 eval=FALSE
###################################################
## data.aov<-aov(DV~AB, data)


###################################################
### chunk number 30: FAKey21 eval=FALSE
###################################################
## AnovaM(data.aov, denoms=c(object), split=list(AB=list("Factor A"=1:2)))


###################################################
### chunk number 31: FAKey22 eval=FALSE
###################################################
## data.aov<-aov(DV~AB, data)


###################################################
### chunk number 32: FAKey23 eval=FALSE
###################################################
## AnovaM(data.aov, type="III", split=list(AB=list("Factor A"=1:2)))


###################################################
### chunk number 33: FAKey24 eval=FALSE
###################################################
## data.aov<-aov(DV~AB, data)


###################################################
### chunk number 34: TEMP
###################################################
op <-options(width=70)


###################################################
### chunk number 35: FAKey25 eval=FALSE
###################################################
## AnovaM(data.aov, denoms=c(c(object)), type="III", split=list(AB= list("Factor A"=1:2)))


###################################################
### chunk number 36: TEMP1
###################################################
options(op)


###################################################
### chunk number 37: FAKey26 eval=FALSE
###################################################
## AnovaM(mainEffects(data.aov, at=B=='B1'), split=list(A=list("Name1"=1, "Name2"=2, ...)))


###################################################
### chunk number 38: FAKey27 eval=FALSE
###################################################
## data$AB<-factor(paste(data$A, data$B, sep="A:B"))
## oneway.test(DV~AB, data, var.equal=F)


###################################################
### chunk number 39: FAKey28 eval=FALSE
###################################################
## data$AB<-factor(paste(data$A, data$B, sep="A:B"))
## kruskal.test(DV~AB, data, var.equal=F)


###################################################
### chunk number 40: FAKey29 eval=FALSE
###################################################
## # calculate mean and se of each combination
## library(gmodels)
## data.means<-with(data, tapply(DV, list(FACTA, FACTB), mean))
## data.se<-with(data, tapply(DV, list(FACTA,FACTB), function(x) ci(x)[4]))
## with(data,interaction.plot(FACTA, FACTB, DV, las=1, lwd=2, 
## ylim=range(pretty(data$DV),na.rm=T), axes=F, xlab="", ylab="", 
## pch=c(16,17), type="b", legend=F))
## # plot the error bars
## arrows(1:3, data.means-data.se, 1:3, data.means+data.se, code=3, angle=90, len=.05)
## # create the axes and their labels
## axis(2, cex.axis=.8,las=1, mgp=c(3,.5,0), tcl=-.2)
## mtext(text="Y-label", side=2, line=3, cex=1)
## axis(1,cex.axis=.8, at=1:3,lab=c("Lab1", "Lab2",...))
## mtext(text="X-label", 1, line=3, cex=1)
## box(bty="l")
## # include a legend
## legend("topright", leg=c("Lab1","Lab2",...), lwd=2, lty=c(2,1), bty="n", 
## pch=c(16,17), cex=1)


###################################################
### chunk number 41: FAKey30 eval=FALSE
###################################################
## # calculate mean and se of each combination
## library(gmodels)
## data.means<-t(tapply(data$DV,list(data$FACTA,data$FACTB),mean, na.rm=T))
## data.se<-t(tapply(data$DV,list(data$FACTA,data$FACTB), function(x) ci(x, na.rm=T)[4]))
## xs <- barplot(data.means,ylim=range(pretty(data$DV), na.rm=T), beside=T, axes=F, xpd=F, 
## axisnames=F, axis.lty=2, legend.text=F, col=c(0,1))
## # plot the error bars
## arrows(xs,data.means,xs,data.means+data.se, code=2, angle=90, len=.05)
## # create the axes and their labels
## axis(2,las=1)
## axis(1,at=apply(xs,2,median), lab=c("Lab1", "Lab2", ...), padj=1, mgp=c(0,0,0))
## mtext(2,text="Y-label",line=3, cex=1)
## mtext(1,text="X-label",line=3, cex=1)
## box(bty="l")
## # include a legend
## legend("topright",leg=c("Lab1", "Lab2", ...), fill=c(0,1), col=c(0,1), bty="n", cex=1)


###################################################
### chunk number 42: FAKey31 eval=FALSE
###################################################
## library(lme4)
## lmer(DV~1+(1|A)+(1|B)+(1|A:B)+..., data)


###################################################
### chunk number 43: FAKey32 eval=FALSE
###################################################
## TukeyHSD(mod, which='Factor')
## #OR better still
## library(multcomp)
## summary(glht(mod, linfct = mcp(Factor = "Tukey")))
## confint(glht(mod, linfct = mcp(Factor = "Tukey")))


###################################################
### chunk number 44: FAKey33 eval=FALSE
###################################################
## plot(data.aov, which=1)


###################################################
### chunk number 45: QuinnReadTable
###################################################
quinn <- read.table('quinn.csv', header=T, sep=',')


###################################################
### chunk number 46: QuinnClass
###################################################
class(quinn$DENSITY)


###################################################
### chunk number 47: QuinnFactor
###################################################
quinn$DENSITY <-factor(quinn$DENSITY)
class(quinn$DENSITY)


###################################################
### chunk number 48: Options
###################################################
op<-options(width=30)


###################################################
### chunk number 49: quinnBoxplot1
###################################################
boxplot(EGGS~DENSITY, quinn)


###################################################
### chunk number 50: quinnBoxplot2
###################################################
boxplot(EGGS~SEASON, quinn)


###################################################
### chunk number 51: quinnBoxplot3
###################################################
boxplot(EGGS~DENSITY * SEASON, quinn)


###################################################
### chunk number 52: Options
###################################################
options(op)


###################################################
### chunk number 53: QuinnReplications
###################################################
replications(EGGS~DENSITY*SEASON, quinn)


###################################################
### chunk number 54: QuinnBalance
###################################################
library(biology)
is.balanced(EGGS~DENSITY*SEASON, quinn)


###################################################
### chunk number 55: QuinnContrasts
###################################################
contrasts(quinn$DENSITY) <- contr.poly(4,scores=c(8,15,30,45))


###################################################
### chunk number 56: QuinnAov
###################################################
quinn.aov <- aov(EGGS~DENSITY+SEASON+DENSITY:SEASON, data=quinn)
#OR equivalently,
quinn.aov <- aov(EGGS~DENSITY*SEASON,data=quinn)


###################################################
### chunk number 57: quinnResidualPlot1
###################################################
plot(quinn.aov, which=1)


###################################################
### chunk number 58: QuinnSummary eval=FALSE
###################################################
## summary(quinn.aov, split=list(DENSITY=list('Linear'=1, 'Quadratic'=2)))


###################################################
### chunk number 59: QuinnSummary2
###################################################
library(biology)
AnovaM(quinn.aov, type="I", split=list(DENSITY=list('Linear'=1, 'Quadratic'=2)))


###################################################
### chunk number 60: quinnPlot
###################################################
# calculate mean and se of each combination
library(gmodels)
quinn.means<-tapply(quinn$EGGS,list(quinn$DENSITY,quinn$SEASON),mean)
quinn.se<-tapply(quinn$EGGS,list(quinn$DENSITY,quinn$SEASON), function(x) ci(x)[4])
# create a numeric version of the density variable
quinn$DENS<-as.numeric(as.character(quinn$DENSITY))
plot(EGGS~DENS, quinn, type="n", axes=F, xlab="", ylab="")
# plot the points and error bars of the winter-spring season
points(quinn.means[,1]~unique(quinn$DENS), pch=16, type="b", lwd=2)
arrows(unique(quinn$DENS), quinn.means[,1]-quinn.se[,1], unique(quinn$DENS), 
quinn.means[,1]+quinn.se[,1], code=3, angle=90, len=.1)
# plot the points and error bars of the summer-autumn season
points(quinn.means[,2]~unique(quinn$DENS),pch=16, type="b", lwd=2, lty=2)
arrows(unique(quinn$DENS), quinn.means[,2]-quinn.se[,2], unique(quinn$DENS), 
quinn.means[,2]+quinn.se[,2], code=3, angle=90, len=.1)
# create the axes and their labels
axis(1,cex.axis=.8)
mtext(text="Adult Density", 1, line=3)
axis(2, cex.axis=.8,las=1)
mtext(text="Egg production", side=2, line=3)
# include a legend
legend("topright",leg=c("Winter-spring","Summer-autumn"),lwd=2,lty=c(1,2), bty="n")
box(bty="l")


###################################################
### chunk number 61: Quinn1ReadTable
###################################################
quinn1 <- read.table('quinn1.csv', header=T, sep=',')


###################################################
### chunk number 62: Quinn1Factor
###################################################
quinn1$DENSITY <-factor(quinn1$DENSITY)


###################################################
### chunk number 63: quinn1Boxplot1
###################################################
boxplot(EGGS~DENSITY * SEASON, quinn1)


###################################################
### chunk number 64: Quinn1Replications
###################################################
replications(EGGS~DENSITY*SEASON, quinn1)
library(biology)
is.balanced(EGGS~DENSITY*SEASON, quinn1)


###################################################
### chunk number 65: Quinn1Contrasts
###################################################
contrasts(quinn1$DENSITY) <- cbind(c(1,-.5,-.5))


###################################################
### chunk number 66: Quinn1AOV
###################################################
quinn1.aov <- aov(EGGS~DENSITY*SEASON,data=quinn1)


###################################################
### chunk number 67: quinn1ResidualPlot1
###################################################
plot(quinn1.aov, which=1)


###################################################
### chunk number 68: Quinn1Anova
###################################################
library(biology)
quinn1.anova<-AnovaM(quinn1.aov, type="I", split=list(DENSITY=
list('6 vs 12&24'=1)))
quinn1.anova


###################################################
### chunk number 69: Options
###################################################
options(op)


###################################################
### chunk number 70: Quinn1MainEffects
###################################################
# effect of density in spring
library(biology)
AnovaM(mainEffects(quinn1.aov, at=SEASON=="spring"), split=list(DENSITY=
list('6 vs 12&24'=1)))
# effect of density in summer
AnovaM(mainEffects(quinn1.aov, at=SEASON=="summer"), split=list(DENSITY=
list('6 vs 12&24'=1)))


###################################################
### chunk number 71: quinn1Plot
###################################################
# calculate mean and se of each combination
library(gmodels)
quinn1.means<-tapply(quinn1$EGGS,list(quinn1$DENSITY,quinn1$SEASON),mean)
quinn1.se<-tapply(quinn1$EGGS,list(quinn1$DENSITY,quinn1$SEASON), function(x) ci(x)[4])
# create a numeric version of the density variable
quinn1$DENS<-as.numeric(as.character(quinn1$DENSITY))
plot(EGGS~DENS, quinn1, type="n", axes=F, xlab="", ylab="")
# plot the points and error bars of the winter-spring season
points(quinn1.means[,1]~unique(quinn1$DENS), pch=16, type="b", lwd=2)
arrows(unique(quinn1$DENS), quinn1.means[,1]-quinn1.se[,1], unique(quinn1$DENS), 
quinn1.means[,1]+quinn1.se[,1], code=3, angle=90, len=.1)
# plot the points and error bars of the summer-autumn season
points(quinn1.means[,2]~unique(quinn1$DENS),pch=16, type="b", lwd=2, lty=2)
arrows(unique(quinn1$DENS), quinn1.means[,2]-quinn1.se[,2], unique(quinn1$DENS), 
quinn1.means[,2]+quinn1.se[,2], code=3, angle=90, len=.1)
# create the axes and their labels
axis(1,cex.axis=.8)
mtext(text="Adult Density", 1, line=3)
axis(2, cex.axis=.8,las=1)
mtext(text="Egg production", side=2, line=3)
# include a legend
legend("topright",leg=c("Winter-spring","Summer-autumn"),lwd=2,lty=c(1,2), bty="n")
box(bty="l")


###################################################
### chunk number 72: MinchReadTable
###################################################
minch <- read.table('minch.csv', header=T, sep=',')


###################################################
### chunk number 73: Options
###################################################
op<-options(width=30)


###################################################
### chunk number 74: minchBoxplot1
###################################################
library(nlme)
minch.agg<-gsummary(minch, 
groups=minch$ZONE:
minch$SITE)
boxplot(LIMPT100~ZONE, 
minch.agg)


###################################################
### chunk number 75: minchBoxplot2
###################################################
boxplot(LIMPT100~SITE, minch)


###################################################
### chunk number 76: minchBoxplot3
###################################################
boxplot(LIMPT100~ZONE*SITE, 
minch)


###################################################
### chunk number 77: Options
###################################################
options(op)


###################################################
### chunk number 78: Options
###################################################
op<-options(width=30)


###################################################
### chunk number 79: minchBoxplot4
###################################################
boxplot(sqrt(LIMPT100)~ZONE, 
minch.agg)


###################################################
### chunk number 80: minchBoxplot5
###################################################
boxplot(sqrt(LIMPT100)~SITE, 
minch)


###################################################
### chunk number 81: minchBoxplot6
###################################################
boxplot(sqrt(LIMPT100)~ZONE*
SITE, minch)


###################################################
### chunk number 82: Options
###################################################
options(op)


###################################################
### chunk number 83: MinchReplications
###################################################
replications(sqrt(LIMPT100)~ZONE*SITE, minch)
library(biology)
is.balanced(sqrt(LIMPT100)~ZONE*SITE, minch)


###################################################
### chunk number 84: MinchContrasts
###################################################
contrasts(minch$ZONE) <- cbind(c(1/3,1/3,-1,1/3))


###################################################
### chunk number 85: MinchAov
###################################################
minch.aov <- aov(sqrt(LIMPT100)~ZONE*SITE,data=minch)


###################################################
### chunk number 86: minchResidualPlot1
###################################################
plot(minch.aov, which=1)


###################################################
### chunk number 87: Options
###################################################
op<-options(width=70)


###################################################
### chunk number 88: MinchAnova
###################################################
library(biology)
(minch.anova<-AnovaM(minch.aov, split = list(ZONE = list('Treed vs No trees' = 
1)), denoms = c("ZONE:SITE","Resid","Resid")))


###################################################
### chunk number 89: Options
###################################################
options(op)


###################################################
### chunk number 90: MinchLMER
###################################################
library(lme4)
lmer(sqrt(LIMPT100)~1+(1|ZONE)+(1|SITE)+(1|ZONE:SITE), minch)


###################################################
### chunk number 91: MincVarCorr
###################################################
minch.var <- VarCorr(lmer(sqrt(LIMPT100)~1+(1|ZONE)+(1|SITE)+(1|ZONE:SITE), minch))
minch.var <- VarCorr(lmer(sqrt(LIMPT100)~ZONE+(1|SITE)+(1|ZONE:SITE), minch))
v.ZS<-round(minch.var$"ZONE:SITE",3)
v.S<-round(minch.var$"SITE",3)
#v.Z<-round(minch.var$"ZONE",3)
v.R<-3.455
v.ZSp <- round((v.ZS*100)/(v.ZS+v.S+v.R),0)
v.Rp <- round((v.R*100)/(v.ZS+v.S+v.R),0)


###################################################
### chunk number 92: minchBarPlot
###################################################
# calculate mean and se of each combination
library(gmodels)
minch.means<-t(tapply(sqrt(minch$LIMPT100),list(minch$ZONE,minch$SITE),mean))
minch.se<-t(tapply(sqrt(minch$LIMPT100),list(minch$ZONE,minch$SITE), 
function(x) ci(x)[4]))  
xs <- barplot(minch.means, ylim=range(sqrt(minch$LIMPT100)), beside=T, axes=F,
xpd=F, axisnames=F, axis.lty=2, legend.text=F, col=c(0,1))	 
# plot the error bars of the winter-spring season
arrows(xs,minch.means,xs,minch.means+minch.se, code=3, angle=90, len=.05)
# create the axes and their labels
axis(2,las=1)
axis(1,at=apply(xs,2,median), lab=c("Land","Mid","Sea\n(-trees)","Sea\n(+trees)"), 
padj=1, mgp=c(0,0,0))
mtext(2,text=expression(paste(sqrt("number of limpets (x100)"))),line=3, cex=1)
mtext(1,text="Zone",line=3, cex=1)
# include a legend
legend("topright",leg=c("Site A", "Site B"), fill=c(0,1), col=c(0,1), bty="n", cex=1)
box(bty="l")


###################################################
### chunk number 93: ReichReadTable
###################################################
reich <- read.table('reich.csv', header=T, sep=',')


###################################################
### chunk number 94: reichBoxplot1
###################################################
boxplot(LEAFAREA~LOCATION * FUNCTION, na.omit(reich))


###################################################
### chunk number 95: ReichReplications
###################################################
replications(LEAFAREA~LOCATION*FUNCTION, reich)
library(biology)
is.balanced(LEAFAREA~LOCATION*FUNCTION, reich)


###################################################
### chunk number 96: ReichContrasts
###################################################
contrasts(reich$LOCATION) <- contr.helmert
contrasts(reich$FUNCTION) <- contr.helmert


###################################################
### chunk number 97: ReichAov
###################################################
reich.aov <- aov(LEAFAREA~LOCATION*FUNCTION,data=reich)


###################################################
### chunk number 98: reichResidualPlot1
###################################################
plot(reich.aov, which=1)


###################################################
### chunk number 99: ReichAnova
###################################################
library(biology)
(reich.anova<-AnovaM(reich.aov,type="III"))


###################################################
### chunk number 100: ReichMainEffects
###################################################
AnovaM(reich.aov.shrub<-mainEffects(reich.aov, at=FUNCTION=="Shrub"), type="III")
library(multcomp)
#Tukey's post-hoc multiple comparisons test
summary(glht(reich.aov.shrub, linfct = mcp(LOCATION = "Tukey")))
#Or as confidence intervals
confint(glht(reich.aov.shrub, linfct = mcp(LOCATION = "Tukey"))) 


###################################################
### chunk number 101: ReichMainEffectsTree
###################################################
AnovaM(reich.aov.tree<-mainEffects(reich.aov, at=FUNCTION=="Tree"), type="III")
library(multcomp)
#Tukey's post-hoc multiple comparisons test
summary(glht(reich.aov.tree, linfct = mcp(LOCATION = "Tukey")))
#Or as confidence intervals
confint(glht(reich.aov.tree, linfct = mcp(LOCATION = "Tukey"))) 


###################################################
### chunk number 102: reichBarPlot
###################################################
# calculate mean and se of each combination
library(gmodels)
reich.means<-t(tapply(reich$LEAFAREA,list(reich$LOCATION,reich$FUNCTION),
mean, na.rm=T))
reich.se<-t(tapply(reich$LEAFAREA,list(reich$LOCATION,reich$FUNCTION), 
function(x) ci(x, na.rm=T)[4]))
xs <- barplot(reich.means,ylim=range(reich$LEAFAREA, na.rm=T),beside=T,
axes=F,xpd=F,axisnames=F,axis.lty=2,legend.text=F, col=c(0,1))
# plot the error bars
arrows(xs,reich.means,xs,reich.means+reich.se, code=2, angle=90, len=.05)
# create the axes and their labels
axis(2,las=1)
axis(1,at=apply(xs,2,median), lab=c("Newmax", "Scarolin", "Venezuel", 
"Wiscons"), padj=1, mgp=c(0,0,0))
mtext(2,text=expression(paste("Mean specific leaf area ", (mm^2))),
line=3, cex=1)
mtext(1,text="Location",line=3, cex=1)
box(bty="l")
# include a legend
legend("topright",leg=c("Shrub", "Tree"), fill=c(0,1), col=c(0,1), bty="n", cex=1)


###################################################
### chunk number 103: Hall1ReadTable
###################################################
hall1 <- read.table('hall1.csv', header=T, sep=',')


###################################################
### chunk number 104: HallFactor
###################################################
hall1$TIME<- as.factor(hall1$TIME)


###################################################
### chunk number 105: hall1Boxplot1
###################################################
boxplot(IND~TREAT * TIME, hall1)


###################################################
### chunk number 106: Options
###################################################
op <- options(width=40)


###################################################
### chunk number 107: hall1Boxplot2
###################################################
boxplot(log(IND+1)~TREAT * TIME, hall1)


###################################################
### chunk number 108: Options
###################################################
options(op)


###################################################
### chunk number 109: Hall1Replications
###################################################
replications(log(IND+1)~TREAT*TIME, hall1)
library(biology)
is.balanced(log(IND+1)~TREAT*TIME, hall1)


###################################################
### chunk number 110: Hall1Combine
###################################################
hall1$TREATTIME <- as.factor(paste(hall1$TREAT,hall1$TIME,sep=""))


###################################################
### chunk number 111: Hall1Anova1
###################################################
contrasts(hall1$TREATTIME) <- cbind(c(1, 1, -1,
-1, 0))
AnovaM(aov(log(IND + 1) ~ TREATTIME, hall1), split = list(TREATTIME = 
list("treatment" = 1)))


###################################################
### chunk number 112: Hall1Anova2
###################################################
contrasts(hall1$TREATTIME) <- cbind(c(1, -1, 1,
-1, 0), c(0, 0, 1, 0, -1))
AnovaM(aov(log(IND + 1) ~ TREATTIME, hall1), split = list(TREATTIME = 
list("time" = 1:2,
" time 2 vs 4" = 1, " time 2 vs 6" = 2)))


###################################################
### chunk number 113: Hall1Anova3
###################################################
contrasts(hall1$TREATTIME) <- cbind(c(1, -1, -1, 
1, 0))
AnovaM(aov(log(IND + 1) ~ TREATTIME, hall1), split = list(TREATTIME = 
list("treatment:time" = 1)))


###################################################
### chunk number 114: hall1Plot
###################################################
# calculate mean and se of each combination
library(gmodels)
hall1.means<-with(hall1, tapply(IND, list(TIME, TREAT), mean))
hall1.se<-with(hall1, tapply(IND, list(TIME,TREAT), function(x) ci(x)[4]))
with(hall1,interaction.plot(TIME, TREAT, IND, las=1, lwd=2, 
ylim=range(pretty(hall1$IND)), axes=F, xlab="", ylab="", pch=c(16,17), type="b", legend=F))
# plot the error bars
arrows(1:3, hall1.means-hall1.se, 1:3, hall1.means+hall1.se, code=3, angle=90, len=.05)
# create the axes and their labels
axis(2, cex.axis=.8,las=1, mgp=c(3,.5,0), tcl=-.2)
mtext(text=expression(paste("Mean number of macroinvertebrate")), side=2, line=3, cex=1)
mtext(text=expression(paste("individuals")), side=2, line=2, cex=1)
axis(1,cex.axis=.8, at=1:3,lab=c("2", "4", "6"))
mtext(text="Time (duration)", 1, line=3, cex=1)
box(bty="l")
# include a legend
legend("topright", leg=c("Control","Nutrient added"), lwd=2, lty=c(2,1), bty="n", 
pch=c(16,17), cex=1)


###################################################
### chunk number 115: MillikenReadTable
###################################################
milliken <- read.table('milliken.csv', header=T, sep=',')


###################################################
### chunk number 116: Options
###################################################
op <- options(width=40)


###################################################
### chunk number 117: millikenBoxplot1
###################################################
boxplot(VOL~FAT*SURF, milliken)


###################################################
### chunk number 118: Options
###################################################
options(op)


###################################################
### chunk number 119: MillikenReplications
###################################################
replications(VOL~FAT*SURF, milliken)
library(biology)
is.balanced(VOL~FAT*SURF, milliken)


###################################################
### chunk number 120: MillikenFactor
###################################################
milliken$FS<-as.factor(paste(milliken$FAT, milliken$SURF, sep=""))


###################################################
### chunk number 121: MillikenAnova1
###################################################
contrasts(milliken$FS)<-cbind(c(1,1,0,0,-1,-1,0), c(0,0,1,1,-1,0,-1))
AnovaM(aov(VOL~FS, milliken),split=list(FS=list("fat"=1:2,
" fat: 1 vs 3" =1," fat 2 vs 3"=2)), type="III")


###################################################
### chunk number 122: MillikenAnova2
###################################################
contrasts(milliken$FS)<-cbind(c(0,0,0,0,0,1,-1), c(0,0,1,-1,1,0,-1))
AnovaM(aov(VOL~FS, milliken),split=list(FS=list("surf"=1:2,
" surf: 2 vs 3" = 1, " surf: 1 vs 3"=2)), type="III")


###################################################
### chunk number 123: MillikenAnova3
###################################################
contrasts(milliken$FS)<-cbind(c(1,-1,0,0,-1,1,0), c(0,0,1,-1,-1,0,1))
AnovaM(aov(VOL~FS, milliken),split=list(FS=list("fat:surf"=1:2,
" fat:surf1"=1, " fat:surf2"=2)), type="III")


###################################################
### chunk number 124: millikenBarPlot
###################################################
# calculate mean and se of each combination
library(gmodels)
milliken.means<-with(milliken,tapply(VOL,list(SURF,FAT),mean, na.rm=T))
milliken.se<-with(milliken,tapply(VOL,list(SURF,FAT), function(x) ci(x, na.rm=T)[4]))
xs <- barplot(milliken.means,ylim=range(milliken$VOL, na.rm=T),beside=T,axes=F
,xpd=F,axisnames=F,axis.lty=2,legend.text=F, col=c(0,1,"gray"))
axis(2,las=1)
axis(1,at=apply(xs,2,median), lab=c("Fat 1", "Fat 2", "Fat 3"), padj=1, mgp=c(0,0,0))
mtext(2,text=expression(paste("Mean specific bread volume ")),line=3, cex=1)
#mtext(1,text="Surfactant type",line=3, cex=1)
box(bty="l")
arrows(xs,milliken.means,xs,milliken.means+milliken.se, code=2, angle=90, len=.05)
legend("topleft",leg=c("Surfactant 1", "Surfactant 2", "Surfactant 3"), 
fill=c(0,1,"gray"), col=c(0,1,"gray"), bty="n", cex=1)


###################################################
### chunk number 125: CreateRFile
###################################################
Stangle("../../factorialAnova.rnw")


