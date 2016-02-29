###################################################
### chunk number 1: Dataframes_SetDir
###################################################
setwd("Dataframes/Data")


###################################################
### chunk number 2: Construct1
###################################################
HABITAT <- factor(c('Mixed','Gipps.Manna','Gipps.Manna','Gipps.Manna','Mixed',
'Mixed','Mixed','Mixed'))   
GST <- c(3.4, 3.4, 8.4, 3.0, 5.6, 8.1, 8.3, 4.6)
EYR <- c(0.0, 9.2, 3.8, 5.0, 5.6, 4.1, 7.1, 5.3)


###################################################
### chunk number 3: Construct2
###################################################
MACNALLY <- data.frame(HABITAT, GST, EYR)
MACNALLY


###################################################
### chunk number 4: Construct3
###################################################
row.names(MACNALLY) <- c('Reedy Lake', 'Pearcedale', 'Warneet', 'Cranbourne', 
'Lysterfield', 'Red Hill', 'Devilbend', 'Olinda')
MACNALLY


###################################################
### chunk number 5: Fix1 eval=FALSE
###################################################
## fix(MACNALLY)


###################################################
### chunk number 6: Options
###################################################
op <-options(width=40)


###################################################
### chunk number 7: Read1
###################################################
MACNALLY <- read.table(
'macnally.csv', header=T, 
row.names=1, sep=',')


###################################################
### chunk number 8: Read2 eval=FALSE
###################################################
## MACNALLY <- read.table(
## 'macnally.txt', header=T, 
## row.names=1, sep='\t')


###################################################
### chunk number 9: Options
###################################################
options(op)


###################################################
### chunk number 10: Read3 eval=FALSE
###################################################
## MACNALLY<-read.table('clipboard', header=T, row.names=1, sep='\t')


###################################################
### chunk number 11: Systat eval=FALSE
###################################################
## library(foreign)
## MACNALLY <- read.systat('macnally.syd', to.data.frame=T)


###################################################
### chunk number 12: Spss eval=FALSE
###################################################
## library(foreign)
## MACNALLY <- read.spss('macnally.sav', to.data.frame=T)


###################################################
### chunk number 13: Minitab eval=FALSE
###################################################
## library(foreign)
## MACNALLY <- as.data.frame(read.mtp('macnally.mtp'))


###################################################
### chunk number 14: Sas eval=FALSE
###################################################
## library(foreign)
## MACNALLY <- read.xport('macnally')


###################################################
### chunk number 15: Write1 eval=FALSE
###################################################
## write.table(MACNALLY, 'macnally.csv', quote=F, row.names=T, sep=',')


###################################################
### chunk number 16: Save
###################################################
#save just the MACNALLY data frame
save(MACNALLY, file='macnally.RData')
#calculate the mean GST
meanGST <- mean(MACNALLY$GST)
#display the mean GST
meanGST
#save the MACNALLY data frame as well as the mean GST object
save(MACNALLY, meanGST, file='macnallystats.RData')


###################################################
### chunk number 17: Load eval=FALSE
###################################################
## load('macnallystats.RData')


###################################################
### chunk number 18: Dump1 eval=FALSE
###################################################
## dump('MACNALLY',file='macnally')


###################################################
### chunk number 19: Dump2 eval=FALSE
###################################################
## dump('MACNALLY',file='')


###################################################
### chunk number 20: Options
###################################################
op <- options(width=65)


###################################################
### chunk number 21: DataFrameVectors
###################################################
MACNALLY$GST


###################################################
### chunk number 22: Options
###################################################
options(op)


###################################################
### chunk number 23: Rm
###################################################
rm(HABITAT,GST,EYR)


###################################################
### chunk number 24: Levels1
###################################################
levels(MACNALLY$HABITAT)


###################################################
### chunk number 25: Factor2
###################################################
FACTOR <- gl(3,2,6,c('low','medium','high'))
FACTOR


###################################################
### chunk number 26: Factor3
###################################################
# examine the default order of levels
levels(MACNALLY$HABITAT)


###################################################
### chunk number 27: Factor3a
###################################################
# redefine the order of levels
MACNALLY$HABITAT<-factor(MACNALLY$HABITAT, levels=c(
'Montane Forest', 'Foothills Woodland','Mixed', 'Gipps.Manna',
'Box-Ironbark','River Red Gum'))


###################################################
### chunk number 28: Factor3b
###################################################
# examine the new order of levels
levels(MACNALLY$HABITAT)


###################################################
### chunk number 29: Factor4
###################################################
# define the factor as ordered
FACTOR <- ordered(FACTOR)
FACTOR


###################################################
### chunk number 30: Subset1 eval=FALSE
###################################################
## #extract all the bird densities from sites that have GST values greater 
## #than 3
## subset(MACNALLY, GST>3)


###################################################
### chunk number 31: Subset2
###################################################
txt <- capture.output( {
#extract all the bird densities from sites that have GST values greater 
#than 3
subset(MACNALLY, GST>3)
} )
if( length(txt) > 10 ){
    txt <- c( txt[1:10], "..." )
}
cat( txt, sep = "\n" )


###################################################
### chunk number 32: Subset3 eval=FALSE
###################################################
## #extract the GST and EYR densities from sites in which GST is greater 
## #than 3
## subset(MACNALLY, GST>3, select=c('GST','EYR'))


###################################################
### chunk number 33: Subset4
###################################################
txt <- capture.output( {
#extract the GST and EYR densities from sites in which GST is greater 
#than 3
subset(MACNALLY, GST>3, select=c('GST','EYR'))
} )
if( length(txt) > 10 ){
    txt <- c( txt[1:10], "..." )
}
cat( txt, sep = "\n" )


###################################################
### chunk number 34: subsetIN
###################################################
#subset the MACNALLY dataset according to those rows that correspond to
#HABITAT 'Montane Forest' or 'Foothills Woodland'
MACNALLY[MACNALLY$HABITAT %in% c("Montane Forest","Foothills Woodland")
,]


###################################################
### chunk number 35: Options
###################################################
op <- options(width=65)


###################################################
### chunk number 36: Tapply
###################################################
#calculate the mean GST densities per HABITAT
tapply(MACNALLY$GST, MACNALLY$HABITAT, mean)


###################################################
### chunk number 37: Aggregate1
###################################################
#calculate the mean GST and EYR densities per habitat
aggregate(MACNALLY[c('GST','EYR')], list(Habitat=MACNALLY$HABITAT), 
mean)


###################################################
### chunk number 38: Gsummary1
###################################################
library(nlme)
gsummary(MACNALLY[c('GST','EYR')],groups=MACNALLY$HABITAT)


###################################################
### chunk number 39: Sorting1 eval=FALSE
###################################################
## MACNALLY[order(MACNALLY$HABITAT,MACNALLY$GST),]


###################################################
### chunk number 40: Sorting2
###################################################
txt <- capture.output( {
MACNALLY[order(MACNALLY$HABITAT,MACNALLY$GST),]
} )
if( length(txt) > 10 ){
    txt <- c( txt[1:10], "..." )
}
cat( txt, sep = "\n" )


###################################################
### chunk number 41: Order10
###################################################
order(MACNALLY$HABITAT,MACNALLY$GST)


###################################################
### chunk number 42: With
###################################################
with(MACNALLY, order(HABITAT, GST))


###################################################
### chunk number 43: Walter1
###################################################
walter<-read.table('walter.csv', header=TRUE, sep=',')
#view first six rows of the walter data set
head(walter)


###################################################
### chunk number 44: Reshape1
###################################################
walter.wide <- reshape(walter, v.names='MITE', timevar='TREAT', idvar='BLOCK', 
direction='wide', drop='LEAVES')
walter.wide


###################################################
### chunk number 45: Reshape2
###################################################
seals <- data.frame(Seal=paste("Site", 1:10, sep=""), T8.00=c(10,35,67,2,49,117,26,85,20,15), T12.00=c(15,47,88,3,46,132,41,101,36,18), T16.00=c(9,31,62,0,39,86,11,3,14,7)) 
seals.long<-reshape(seals, varying=c("T8.00", "T12.00", "T16.00"), v.names="Count",timevar="TIME",times=paste("T",seq(8,16,by=4),sep=""),idvar="Seal",direction='long')
seals.long


###################################################
### chunk number 46: DataSetExample
###################################################
# create the response variable with four sets of 10 random numbers from
# a normal distribution
GROWTH.RATE <- c(rnorm(10, 250,20), rnorm(10, 250,20),rnorm(10, 250,20),
rnorm(10, 250,20))
# create the nitrogen treatment factor with four levels each replicated 
#10 times
TREATMENT <- gl(4,10,40,c('N1', 'N2', 'N3', 'N4'))
# combine the vectors into a dataframe
NITROGEN <- data.frame(GROWTH.RATE, TREATMENT)


###################################################
### chunk number 47: DataSetExample2
###################################################
# create the nitrogen treatment factor with four levels
TREATMENT <- c("N1","N2","N3","N4")
# create the season factor with two levels
SEASON <- c("WINTER", "SUMMER")
# use the expand.grid function to create a dataframe with each 
# combination replicated 5 times
TS<-expand.grid(TREATMENT=TREATMENT,SEASON=SEASON, reps=1:5)
# combine a normally distributed response variable to the factor
# combinations using the data.frame function
NITROGEN<-data.frame(TS,GROWTH.RATE=rnorm(40,250,20))


###################################################
### chunk number 48: CreateRFile
###################################################
Stangle("../../dataframes.rnw")


