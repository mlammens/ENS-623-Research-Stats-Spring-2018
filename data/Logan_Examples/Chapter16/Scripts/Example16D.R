###################################################
### chunk number 1: pg 481
###################################################
H0.tab <- matrix(c(0.7*0.4,0.7*0.6,0.3*0.4,0.3*0.6),nrow=2)
rownames(H0.tab) <- c("Aust","Bemb")
colnames(H0.tab) <- c("Empty","Occupied")
library(epitools)
table.margins(H0.tab)


###################################################
### chunk number 2: pg 482
###################################################
HA.tab <- matrix(c(0.7*0.4,0.7*0.6,0.3*0.5,0.3*0.5),nrow=2)
rownames(HA.tab) <- c("Aust","Bemb")
colnames(HA.tab) <- c("Empty","Occupied")
table.margins(HA.tab)


###################################################
### chunk number 3: pg 482
###################################################
ws <- sqrt(chisq.test(as.vector(HA.tab), p=as.vector(H0.tab))$stat[[1]])


###################################################
### chunk number 4: pg 482
###################################################
library(pwr)
pwr.chisq.test(df=1, w=ws, power=0.8)
