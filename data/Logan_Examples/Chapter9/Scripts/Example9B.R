###################################################
### chunk number 1: pg 230
###################################################
paruelo <- read.table('paruelo.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 230
###################################################
library(car)
scatterplot.matrix(~C3+MAP+MAT+JJAMAP+DJFMAP+LONG+LAT, data=paruelo, diag="boxplot")


###################################################
### chunk number 3: pg 231
###################################################
scatterplot.matrix(~log10(C3+0.1)+MAP+MAT+JJAMAP+DJFMAP+LONG+LAT, data=paruelo, diag="boxplot")


###################################################
### chunk number 4: pg 232
###################################################
cor(paruelo[,2:7])


###################################################
### chunk number 5: pg 232
###################################################
vif(lm(log10(C3+.1)~MAP+MAT+JJAMAP+DJFMAP+LONG+LAT, data=paruelo))
1/vif(lm(log10(C3+.1)~MAP+MAT+JJAMAP+DJFMAP+LONG+LAT, data=paruelo))


###################################################
### chunk number 6: pg 233
###################################################
1/vif(lm(log10(C3+.1)~LAT+LONG+LAT:LONG, data=paruelo))


###################################################
### chunk number 7: pg 233
###################################################
paruelo$cLAT <- paruelo$LAT-mean(paruelo$LAT)
#OR
paruelo$cLAT <- scale(paruelo$LAT, scale=F)
paruelo$cLONG <- scale(paruelo$LONG, scale=F)
1/vif(lm(log10(C3+.1)~cLAT+cLONG+cLAT:cLONG, data=paruelo))


###################################################
### chunk number 8: pg 233
###################################################
paruelo.lm<-lm(log10(C3+.1)~cLAT+cLONG+cLAT:cLONG, data=paruelo)
plot(paruelo.lm)


###################################################
### chunk number 9: pg 234
###################################################
influence.measures(paruelo.lm)


###################################################
### chunk number 10: pg 234
###################################################
summary(paruelo.lm)


###################################################
### chunk number 11: pg 235
###################################################
LAT_sd1<-mean(paruelo$cLAT)-2*sd(paruelo$cLAT)
paruelo_LONG.lm1<-lm(log10(C3+.1)~cLONG*c(cLAT-LAT_sd1), data=paruelo)
summary(paruelo_LONG.lm1)


###################################################
### chunk number 12: pg 235
###################################################
LAT_sd2<-mean(paruelo$cLAT)-1*sd(paruelo$cLAT)
paruelo_LONG.lm2<-lm(log10(C3+.1)~cLONG*c(cLAT-LAT_sd2), data=paruelo)
summary(paruelo_LONG.lm2)


###################################################
### chunk number 13: pg 236
###################################################
LAT_sd4<-mean(paruelo$cLAT)-1*sd(paruelo$cLAT)
paruelo_LONG.lm4<-lm(log10(C3+.1)~cLONG*c(cLAT-LAT_sd4), data=paruelo)
summary(paruelo_LONG.lm4)


###################################################
### chunk number 14: pg 237
###################################################
LAT_sd5<-mean(paruelo$cLAT)-1*sd(paruelo$cLAT)
paruelo_LONG.lm5<-lm(log10(C3+.1)~cLONG*c(cLAT-LAT_sd5), data=paruelo)
summary(paruelo_LONG.lm5)
