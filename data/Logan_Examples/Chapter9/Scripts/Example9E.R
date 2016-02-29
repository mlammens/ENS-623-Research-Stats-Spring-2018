###################################################
### chunk number 1: pg 241
###################################################
mckechnie2 <- read.table('mckechnie2.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 242
###################################################
library(car)
vif(lm(HK~PRECIP+MAXTEMP+MINTEMP+ALT, mckechnie2))


###################################################
### chunk number 3: pg 242
###################################################
mckechnie2.lm<-lm(HK~PRECIP+MAXTEMP+MINTEMP+ALT, mckechnie2)
summary(mckechnie2.lm)


###################################################
### chunk number 4: pg 243
###################################################
stat <- function(data, indices) {
summary(lm(HK~PRECIP+MAXTEMP+MINTEMP+ALT, data))$coef[,3]
}


###################################################
### chunk number 5: pg 243
###################################################
rand.gen<- function(data,mle) {
out<-data
out$HK <-sample(out$HK, replace=F)
out
}


###################################################
### chunk number 6: pg 243
###################################################
library(boot)
mckechnie2.boot<-boot(mckechnie2, stat, R=1000, sim="parametric", ran.gen=rand.gen)


###################################################
### chunk number 7: pg 243
###################################################
t<-apply(apply(abs(mckechnie2.boot$t), 1, ">=", abs(mckechnie2.boot$t0))*1, 1, "sum")+1
t/(mckechnie2.boot$R+1)


###################################################
### chunk number 8: pg 243
###################################################
stat <- function(data, indices) {
summary(lm(HK~PRECIP+MAXTEMP+MINTEMP+ALT, data))$fstatistic
}
rand.gen<- function(data,mle) {
out<-data
out$HK <-sample(out$HK, replace=F)
out
}
mckechnie2.boot<-boot(mckechnie2, stat, R=1000, sim="parametric", ran.gen=rand.gen)
f<-apply(apply(abs(mckechnie2.boot$t), 1, ">=", abs(mckechnie2.boot$t0))*1, 1, "sum")+1
f/(mckechnie2.boot$R+1)
