###################################################
### chunk number 1: pg 76
###################################################
sample(1:37, 5, replace=F)


###################################################
### chunk number 2: pg 76
###################################################
MACNALLY <- read.table("macnally.csv", header=T, sep=",")
sample(row.names(MACNALLY), 5, replace=F)


###################################################
### chunk number 3: pg 77
###################################################
sample(seq(0,600, by=30), 10, replace=F)


###################################################
### chunk number 4: pg 78
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
### chunk number 5: pg 78
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
### chunk number 6: pg 78
###################################################
data.frame(X=runif(10,0,100), Y=runif(10,0,200))


###################################################
### chunk number 7: pg 79
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
### chunk number 8: pg 80
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
### chunk number 9: pg 82
###################################################
DIST <- matrix(runif(40,0,100),nrow=10)
DIST


###################################################
### chunk number 10: pg 82
###################################################
rownames(DIST) <- paste("Transect", 1:10, sep='')
colnames(DIST) <- paste("Day", 1:4, sep='')
round(DIST, digits=2)


###################################################
### chunk number 11: pg 82
###################################################
X <- c(0.77,0.5,0.55,0.45,0.4, 0.2, 0.05)
Y <- c(0.9,0.9,0.7,0.45,0.2,0.1,0.3)
XY <- cbind(X,Y)
library(sp)
XY.line <- Line(XY)
XY.points <- spsample(XY.line,n=10,'random')
plot(XY, type="l")
points(XY.points)


###################################################
### chunk number 12: pg 83
###################################################
coordinates(XY.points)


###################################################
### chunk number 13: pg 83
###################################################
TREATMENTS <- gl(4,6,24,c('A','B','C','D'))
matrix(sample(TREATMENTS),nrow=4)


###################################################
### chunk number 14: pg 84
###################################################
TREATMENTS <- replicate(6,sample(c('A','B','C','D')))
colnames(TREATMENTS) <- paste('Block',1:6,sep='')
TREATMENTS



