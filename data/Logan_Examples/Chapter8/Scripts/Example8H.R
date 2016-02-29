###################################################
### chunk number 1: pg 203
###################################################
mckechnie <- read.table('mckechnie.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 203
###################################################
stat<-function(data,index) {
summary(lm(HK~ALT, data))$coef[2,3]
}


###################################################
### chunk number 3: pg 203
###################################################
rand.gen<- function(data,mle) {
out<-data
out$ALT <-sample(out$ALT, replace=F)
out
}


###################################################
### chunk number 4: pg 203
###################################################
library(boot)


###################################################
### chunk number 5: pg 204
###################################################
mckechnie.boot<-boot(mckechnie, stat, R=5000, sim="parametric", ran.gen=rand.gen)


###################################################
### chunk number 6: pg 204
###################################################
plot(mckechnie.boot)


###################################################
### chunk number 7: pg 204
###################################################
mckechnie.boot


###################################################
### chunk number 8: pg 204
###################################################
t<-length(mckechnie.boot$t[mckechnie.boot$t >= mckechnie.boot$t0])+1
t/(mckechnie.boot$R+1)


###################################################
### chunk number 9: pg 205
###################################################
par.boot<-function(mckechnie,index) {
x<-mckechnie$ALT[index]
y<-mckechnie$HK[index]
model<-lm(y~x)
coef(model)
}


###################################################
### chunk number 10: pg 205
###################################################
mckechnie.boot<-boot(mckechnie, par.boot, R=5000)


###################################################
### chunk number 11: pg 205
###################################################
mckechnie.boot


###################################################
### chunk number 12: pg 205
###################################################
boot.ci(mckechnie.boot, index=2)


###################################################
### chunk number 13:pg 206
###################################################
pred.boot<-function(mckechnie,index) {
mckechnie.rs<-mckechnie[index,]
mckechnie.lm<-lm(HK~ALT, mckechnie.rs)
predict(mckechnie.lm, data.frame(ALT=1))
}


###################################################
### chunk number 14: pg 206
###################################################
mckechnie.boot<-boot(mckechnie, pred.boot, R=5000)


###################################################
### chunk number 15: pg 206
###################################################
mckechnie.boot


###################################################
### chunk number 16: pg 206
###################################################
boot.ci(mckechnie.boot, index=1)
