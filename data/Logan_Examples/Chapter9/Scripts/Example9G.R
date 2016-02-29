###################################################
### chunk number 1: pg 248
###################################################
peake <- read.table('peake.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 248
###################################################
library(car)
scatterplot(SPECIES~AREA, data=peake)


###################################################
### chunk number 3: pg 248
###################################################
plot(lm(SPECIES~AREA, data=peake), which=1)


###################################################
### chunk number 4: pg 249
###################################################
peake.nls <- nls(SPECIES~alpha*AREA^beta, start=list(alpha=0.1, beta=1), peake)


###################################################
### chunk number 5: pg 249
###################################################
plot(resid(peake.nls)~fitted(peake.nls))


###################################################
### chunk number 6: pg 249
###################################################
summary(peake.nls)


###################################################
### chunk number 7: pg 249
###################################################
AIC(peake.nls, k=log(nrow(peake))) #BIC
AIC(peake.nls) #AIC
deviance(peake.nls)/df.residual(peake.nls) #MSresid
peake.lm<-lm(SPECIES~AREA, data=peake) #linear fit
AIC(peake.lm, k=log(nrow(peake))) #lm BIC
AIC(peake.lm) #lm AIC
deviance(peake.lm)/df.residual(peake.lm) #lm MSresid


###################################################
### chunk number 8: pg 250
###################################################
peake.nls1 <- nls(SPECIES~SSasymp(AREA,a,b,c),peake)
summary(peake.nls1)
AIC(peake.nls1) #AIC
deviance(peake.nls1)/df.residual(peake.nls1) #MSresid
anova(peake.nls,peake.nls1)


###################################################
### chunk number 9: 251
###################################################
plot(SPECIES~AREA, peake, pch=16, axes=F, xlab="", ylab="")
axis(1, cex.axis=.8)
mtext(text=expression(paste("Clump area ", (dm^2))), side=1, line=3)
axis(2, las=1)
mtext(text="Number of species", side=2, line=3)
box(bty="l")
x <- seq(0,30000, l=1000)
points(x, predict(peake.nls, data.frame(AREA=x)), type='l', lty=2)
points(x, predict(nls(SPECIES~SSasymp(AREA,a,b,c),peake), data.frame(AREA=x)), type='l', lty=1)
box(bty="l")
