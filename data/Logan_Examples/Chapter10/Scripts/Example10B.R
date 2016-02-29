###################################################
### chunk number 1: pg 268
###################################################
keough <- read.table('keough.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 269
###################################################
boxplot(SERP~BIOFILM, data=keough)


###################################################
### chunk number 3: pg 269
###################################################
with(keough, plot(tapply(SERP, BIOFILM, mean), tapply(SERP, BIOFILM, var)))


###################################################
### chunk number 4: pg 269
###################################################
boxplot(log10(SERP)~BIOFILM, data=keough)


###################################################
### chunk number 5: pg 269
###################################################
with(keough, plot(tapply(log10(SERP), BIOFILM, mean), tapply(log10(SERP), BIOFILM, var)))


###################################################
### chunk number 6: pg 270
###################################################
contrasts(keough$BIOFILM) <- cbind(c(0,1,0,-1), c(2,-1,0,-1), c(-1,-1,3,-1))


###################################################
### chunk number 7: pg 270
###################################################
round(crossprod(contrasts(keough$BIOFILM)),2)


###################################################
### chunk number 8: pg 270
###################################################
keough.list <- list(BIOFILM=list("NL vs UL"=1, "F vs (NL&UL)"=2, "SL vs (F&NL&UL)"=3))


###################################################
### chunk number 9: pg 270
###################################################
keough.aov <- aov(log10(SERP)~BIOFILM, data=keough)


###################################################
### chunk number 10: pg 270
###################################################
plot(keough.aov)


###################################################
### chunk number 11: pg 271
###################################################
summary(keough.aov, split=keough.list)


###################################################
### chunk number 12: pg 271
###################################################
means <- with(keough, tapply(SERP, BIOFILM, mean, na.rm =T))
sds <- with(keough, tapply(SERP, BIOFILM, sd, na.rm =T))
n <- with(keough, tapply(SERP, BIOFILM, length))
ses <- sds/sqrt(n)
ys <- pretty(c(means - ses, means + (2 * ses)))
xs<-barplot(means, beside=T, axes=F, ann=F, ylim = c(min(ys), max(ys)), xpd=F)
arrows(xs, means+ses, xs, means-ses, ang=90, length=0.1, code=3)
axis(2, las = 1)
mtext(2, text = "Mean number of serpulids", line = 3, cex = 1.5)
mtext(1, text = "Biofilm treatment", line = 3, cex = 1.5)
box(bty="l")

