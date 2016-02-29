###################################################
### chunk number 1: pg 48
###################################################
HABITAT <- factor(c('Mixed','Gipps.Manna','Gipps.Manna','Gipps.Manna','Mixed',
'Mixed','Mixed','Mixed'))   
GST <- c(3.4, 3.4, 8.4, 3.0, 5.6, 8.1, 8.3, 4.6)
EYR <- c(0.0, 9.2, 3.8, 5.0, 5.6, 4.1, 7.1, 5.3)


###################################################
### chunk number 2: pg 49
###################################################
MACNALLY <- data.frame(HABITAT, GST, EYR)
MACNALLY


###################################################
### chunk number 3: pg 49
###################################################
row.names(MACNALLY) <- c('Reedy Lake', 'Pearcedale', 'Warneet', 'Cranbourne', 
'Lysterfield', 'Red Hill', 'Devilbend', 'Olinda')
MACNALLY


###################################################
### chunk number 4: pg 50
###################################################
fix(MACNALLY)


###################################################
### chunk number 5: pg 51
###################################################
MACNALLY <- read.table('macnally.csv', header=T, row.names=1, sep=',')


###################################################
### chunk number 6: pg 51
###################################################
MACNALLY <- read.table('macnally.txt', header=T, row.names=1, sep='\t')


###################################################
### chunk number 7: pg 51
###################################################
MACNALLY<-read.table('clipboard', header=T, row.names=1, sep='\t')


###################################################
### chunk number 8: pg 52 (SYSTAT)
###################################################
library(foreign)
MACNALLY <- read.systat('macnally.syd', to.data.frame=T)


###################################################
### chunk number 9: pg 52 (SPSS)
###################################################
library(foreign)
MACNALLY <- read.spss('macnally.sav', to.data.frame=T)


###################################################
### chunk number 10: pg 52 (Minitab)
###################################################
library(foreign)
MACNALLY <- as.data.frame(read.mtp('macnally.mtp'))


###################################################
### chunk number 11: pg 52 (SAS)
###################################################
library(foreign)
MACNALLY <- read.xport('macnally')


###################################################
### chunk number 12: pg 53
###################################################
write.table(MACNALLY, 'macnally.csv', quote=F, row.names=T, sep=',')


###################################################
### chunk number 13:pg 53
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
### chunk number 14: pg 53
###################################################
load('macnallystats.RData')


###################################################
### chunk number 15: pg 53
###################################################
dump('MACNALLY',file='macnally')


###################################################
### chunk number 16: pg 54
###################################################
dump('MACNALLY',file='')



###################################################
### chunk number 17: pg 54
###################################################
MACNALLY$GST


###################################################
### chunk number 18: pg 54
###################################################
rm(HABITAT,GST,EYR)


###################################################
### chunk number 19: pg 55
###################################################
levels(MACNALLY$HABITAT)


###################################################
### chunk number 20: pg 55
###################################################
FACTOR <- gl(3,2,6,c('low','medium','high'))
FACTOR


###################################################
### chunk number 21: pg 55
###################################################
# examine the default order of levels
levels(MACNALLY$HABITAT)


###################################################
### chunk number 22: pg 55
###################################################
# redefine the order of levels
MACNALLY$HABITAT<-factor(MACNALLY$HABITAT, levels=c(
'Montane Forest', 'Foothills Woodland','Mixed', 'Gipps.Manna',
'Box-Ironbark','River Red Gum'))


###################################################
### chunk number 23: pg 55
###################################################
# examine the new order of levels
levels(MACNALLY$HABITAT)


###################################################
### chunk number 24: pg 56
###################################################
# define the factor as ordered
FACTOR <- ordered(FACTOR)
FACTOR


###################################################
### chunk number 25: pg 56
###################################################
#extract all the bird densities from sites that have GST values greater 
#than 3
subset(MACNALLY, GST>3)


###################################################
### chunk number 26: pg 57
###################################################
#extract all the bird densities from sites that have GST values greater 
#than 3
subset(MACNALLY, GST>3)


###################################################
### chunk number 27: pg 57
###################################################
#extract the GST and EYR densities from sites in which GST is greater 
#than 3
subset(MACNALLY, GST>3, select=c('GST','EYR'))


###################################################
### chunk number 28: pg 57
###################################################
#subset the MACNALLY dataset according to those rows that correspond to
#HABITAT 'Montane Forest' or 'Foothills Woodland'
MACNALLY[MACNALLY$HABITAT %in% c("Montane Forest","Foothills Woodland"),]


###################################################
### chunk number 29: pg 58
###################################################
#calculate the mean GST densities per HABITAT
tapply(MACNALLY$GST, MACNALLY$HABITAT, mean)


###################################################
### chunk number 30: pg 58
###################################################
#calculate the mean GST and EYR densities per habitat
aggregate(MACNALLY[c('GST','EYR')], list(Habitat=MACNALLY$HABITAT), mean)


###################################################
### chunk number 31: pg 58
###################################################
library(nlme)
gsummary(MACNALLY[c('GST','EYR')],groups=MACNALLY$HABITAT)


###################################################
### chunk number 32: pg 59
###################################################
MACNALLY[order(MACNALLY$HABITAT,MACNALLY$GST),]


###################################################
### chunk number 33: pg 59
###################################################
order(MACNALLY$HABITAT,MACNALLY$GST)


###################################################
### chunk number 34: pg 59
###################################################
with(MACNALLY, order(HABITAT, GST))


###################################################
### chunk number 35: pg 60
###################################################
walter<-read.table('walter.csv', header=TRUE, sep=',')
#view first six rows of the walter data set
head(walter)


###################################################
### chunk number 36: pg 60
###################################################
walter.wide <- reshape(walter, v.names='MITE', timevar='TREAT', idvar='BLOCK', direction='wide', drop='LEAVES')
walter.wide


###################################################
### chunk number 37: pg 61
###################################################
seals <- data.frame(Seal=paste("Site", 1:10, sep=""), T8.00=c(10,35,67,2,49,117,26,85,20,15), T12.00=c(15,47,88,3,46,132,41,101,36,18), T16.00=c(9,31,62,0,39,86,11,3,14,7)) 
seals.long<-reshape(seals, varying=c("T8.00", "T12.00", "T16.00"), v.names="Count",timevar="TIME",times=paste("T",seq(8,16,by=4),sep=""),idvar="Seal",direction='long')
seals.long


###################################################
### chunk number 38: pg 64
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
### chunk number 39: pg 64
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



