###################################################
### chunk number 1: Sampling_SetDir
###################################################
setwd("Sampling/Data")


###################################################
### chunk number 2: Sample1
###################################################
sample(1:37, 5, replace=F)


###################################################
### chunk number 3: Sample2
###################################################
MACNALLY <- read.table("macnally.csv", header=T, sep=",")
sample(row.names(MACNALLY), 5, replace=F)


###################################################
### chunk number 4: Sample3
###################################################
sample(seq(0,600, by=30), 10, replace=F)


###################################################
### chunk number 5: sample4
###################################################
# First step is to obtain a random starting (first survey) time.
# To do this retain the minimum time from a random set of times
# between 1 (minute) and 60*10 (number of minutes in 10 hours)
TIMES <- min(runif(20,1,60*10))
# Next we calculate additional random times each of which is a minimum
# and maximum of 30 and 60 minutes respectively after the previous
for(i in 2:20) {
TIMES[i] <- runif(1,TIMES[i-1]+30,TIMES[i-1]+60)
if(TIMES[i]>9*60) break
}
# Randomly select 10 of these times
TIMES <- sample(TIMES, 10, replace=F)
# Generate a Site name for the times
names(TIMES) <- paste('Site',1:10, sep='')
# Finally sort the list and put it in a single column
cbind('Times'=sort(TIMES))


###################################################
### chunk number 6: sample5
###################################################
# Convert these minutes into hs, mins, seconds
hrs <- TIMES%/%60
mins <- trunc(TIMES%%60)
secs <- trunc(((TIMES%%60)-mins)*60)
RelTm <- paste(hrs,sprintf("%2.0f",mins),secs, sep=":")
# We could also express them as real times
# If sundown occurs at 18:00 (18*60*60 seconds)
RealTm<-format(strptime(RelTm, "%H:%M:%S")+(18*60*60), "%H:%M:%S")
# Finally sort the list and put it in a single column
data.frame('Minutes'=sort(TIMES), 'RelativeTime'=RelTm[order(TIMES)],
RealTime=RealTm[order(TIMES)])


###################################################
### chunk number 7: Sample5
###################################################
data.frame(X=runif(10,0,100), Y=runif(10,0,200))


###################################################
### chunk number 8: Map1
###################################################
getOption("SweaveHooks")[["fig"]]()
options(SweaveHooks=list(fig=function() par(mfrow=c(1,1),mar=c(4,5,1,1))))
LAT <- c(145.450, 145.456, 145.459, 145.457, 145.451, 145.450)
LONG <- c(37.525, 37.526, 37.528, 37.529, 37.530,37.525)
XY <- cbind(LAT,LONG)
op <- par(mar=c(4,5,1,1))
#plot(XY) 
plot(XY, type='l',xlim=c(145.449,145.46),asp=1,panel.first=grid(11),axes=F,xaxs='i',tcl=0,xlab='LATITUDE',ylab='')
polygon(XY,col='gray95',border='black')
grid(11)
box()
#axis(1, at=pretty(XY[,1])[2:5]))
axis(1, at=pretty(XY[,1])[1:5],cex.axis=.75)
axis(2,cex.axis=.75,las=1)
mtext('LATITUDE',2,line=4)
par(op)


###################################################
### chunk number 9: Sample6
###################################################
LAT <- c(145.450, 145.456, 145.459, 145.457, 145.451, 145.450)
LONG <- c(37.525, 37.526, 37.528, 37.529, 37.530,37.525)
XY <- cbind(LAT,LONG)
plot(XY, type='l')
library(sp)
XY.poly <- Polygon(XY)
XY.points <- spsample(XY.poly, n=8, type='random')
XY.points


###################################################
### chunk number 10: Map2
###################################################
getOption("SweaveHooks")[["fig"]]()
options(SweaveHooks=list(fig=function() par(mfrow=c(1,1),mar=c(4,5,1,1))))
plot(XY, type='l',xlim=c(145.449,145.46),asp=1,panel.first=grid(11),axes=F,xaxs='i',tcl=0,xlab='LATITUDE',ylab='')
polygon(XY,col='gray95',border='black')
grid(11)
box()
axis(1, at=pretty(XY[,1])[1:5],cex.axis=.75)
axis(2,cex.axis=.75,las=1)
mtext('LATITUDE',2,line=4)
points(XY.points[1:5])
par(op)


###################################################
### chunk number 11: PointsOnMap eval=FALSE
###################################################
## points(XY.points[1:5])


###################################################
### chunk number 12: Sample7
###################################################
LAT1 <- c(145.450, 145.456, 145.457, 145.451,145.450)
LONG1 <- c(37.525, 37.526, 37.529, 37.530, 37.525)
XY1 <- cbind(LAT1,LONG1)
LAT2 <- c(145.456,145.459,145.457,145.456)
LONG2 <- c(37.526, 37.528, 37.529,37.526)
XY2 <- cbind(LAT2,LONG2)
library(sp)
XY1.poly <- Polygon(XY1)
XY1.polys <- Polygons(list(XY1.poly), "Heathland")
XY2.poly <- Polygon(XY2)
XY2.polys <- Polygons(list(XY2.poly), "Swamp")
XY.Spolys <- SpatialPolygons(list(XY1.polys, XY2.polys))
XY.Spoints <- spsample(XY.Spolys, n=10, type='stratified')
XY.Spoints


###################################################
### chunk number 13: Map3
###################################################
getOption("SweaveHooks")[["fig"]]()
op <- par(mar=c(1,1,1,1),mfrow=c(2,2))
plot(XY.Spolys, col=c('gray95','gray90'))
box()
grid(10)
points(spsample(XY.Spolys, n=10, type='random'),pch=16)
text(145.45,37.531,'Random sampling',cex=1,pos=4)
plot(XY.Spolys, col=c('gray95','gray90'))
box()
grid(10)
points(spsample(XY.Spolys, n=10, type='stratified'),pch=16)
#s
#points(s,pch=16)
text(145.45,37.531,'Stratified random',cex=1,pos=4)
plot(XY.Spolys, col=c('gray95','gray90'))
box()
grid(10)
points(spsample(XY.Spolys, n=10, type='regular'),pch=16)
text(145.45,37.531,'Systematic sampling',cex=1,pos=4)
plot(XY.Spolys, col=c('gray95','gray90'))
box()
grid(10)
points(spsample(XY.Spolys, n=10, type='nonaligned'),pch=16)
text(145.45,37.531,'Nonaligned systematic',cex=1,pos=4)
par(op)


###################################################
### chunk number 14: RandDist
###################################################
DIST <- matrix(runif(40,0,100),nrow=10)
DIST


###################################################
### chunk number 15: RandDist1
###################################################
rownames(DIST) <- paste("Transect", 1:10, sep='')
colnames(DIST) <- paste("Day", 1:4, sep='')
round(DIST, digits=2)


###################################################
### chunk number 16: Map4
###################################################
getOption("SweaveHooks")[["fig"]]()
X <- c(0.77,0.5,0.55,0.45,0.4, 0.2, 0.05)
Y <- c(0.9,0.9,0.7,0.45,0.2,0.1,0.3)
XY <- cbind(X,Y)
library(sp)
XY.line <- Line(XY)
XY.points <- spsample(XY.line,n=10,'random')
plot(XY, type="l")
points(XY.points)


###################################################
### chunk number 17: Map5a
###################################################
coordinates(XY.points)


###################################################
### chunk number 18: Treatments1
###################################################
TREATMENTS <- gl(4,6,24,c('A','B','C','D'))
matrix(sample(TREATMENTS),nrow=4)


###################################################
### chunk number 19: Treatments2
###################################################
TREATMENTS <- replicate(6,sample(c('A','B','C','D')))
colnames(TREATMENTS) <- paste('Block',1:6,sep='')
TREATMENTS


###################################################
### chunk number 20: CreateRFile
###################################################
Stangle("../../Sampling.rnw")


