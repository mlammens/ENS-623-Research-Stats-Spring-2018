###################################################
### chunk number 1: pg 479
###################################################
roberts <- read.table('roberts.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 479
###################################################
roberts.xtab <- table(roberts$POSITION,roberts$DEAD)
#OR
roberts.xtab <- with(roberts,table(POSITION,DEAD))
roberts.xtab


###################################################
### chunk number 3: pg 479
###################################################
chisq.test(roberts.xtab,corr=F)$exp


###################################################
### chunk number 4: pg 479
###################################################
chisq.test(roberts.xtab,corr=F)
library(biology)
g.test(roberts.xtab,corr="williams")


###################################################
### chunk number 5: pg 480
###################################################
chisq.test(roberts.xtab,corr=F)$res


###################################################
### chunk number 6: pg 480
###################################################
# compare each of the positions pairwise to the bottom position
#add a small constant to remove the 0 value
library(biology)
oddsratios(roberts.xtab+.5)


###################################################
### chunk number 7: pg 481
###################################################
library(vcd)
strucplot(roberts.xtab, shade=T, labeling_args=list(set_varnames=c(POSITION="Transect position", DEAD="Dead coolibah trees"), offset_varnames = c(left = 1.5, top=1.5)), margins=c(5,2,2,5))
