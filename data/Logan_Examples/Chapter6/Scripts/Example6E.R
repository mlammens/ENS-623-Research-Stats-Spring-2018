###################################################
### chunk number 1: pg 148
###################################################
beetles <- read.table('beetle.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 148
###################################################
boxplot(BEETLES~SIZE, beetles)


###################################################
### chunk number 3: pg 148
###################################################
boxplot(sqrt(BEETLES)~SIZE, beetles)


###################################################
### chunk number 4: 149
###################################################
stat <- function(data, indices) {
t.test <- t.test(BEETLES~SIZE, data)$"stat"
t.test
}


###################################################
### chunk number 5: 149
###################################################
rand.gen <- function(data,mle) {
out <- data
out$SIZE <- sample(out$SIZE, replace=F)
out
}


###################################################
### chunk number 6: pg 149
###################################################
library(boot)
beetles.boot <- boot(beetles, stat, R=5000, sim="parametric", ran.gen=rand.gen)


###################################################
### chunk number 7: pg 149
###################################################
print(beetles.boot)


###################################################
### chunk number 8: pg 149
###################################################
plot(beetles.boot)


###################################################
### chunk number 9: pg 150
###################################################
tval <- length(beetles.boot[beetles.boot$t >= abs(beetles.boot$t0)])+1
tval/(beetles.boot$R + 1)
