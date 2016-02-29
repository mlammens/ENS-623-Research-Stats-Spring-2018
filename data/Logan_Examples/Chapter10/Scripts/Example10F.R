##################################################
### chunk number 1: pg 277
###################################################
sanchez <- read.table('sanchez.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 277
###################################################
boxplot(GUANO~COLTYPE, data=sanchez)


###################################################
### chunk number 3: pg 277
###################################################
boxplot(sqrt(GUANO)~COLTYPE, data=sanchez)


###################################################
### chunk number 4: pg 277
###################################################
oneway.test(sqrt(GUANO)~COLTYPE, data=sanchez)


###################################################
### chunk number 5: pg 277
###################################################
pairwise.t.test(sqrt(sanchez$GUANO), sanchez$COLTYPE, pool.sd=F, p.adj="holm")


###################################################
### chunk number 6: pg 278
###################################################
pvalues <- pairwise.t.test(sqrt(sanchez$GUANO), sanchez$COLTYPE, pool.sd=F, p.adj="none")$p.value
pvalues


###################################################
### chunk number 7: pg 278
###################################################
library(multtest)
mt.rawp2adjp(pvalues,proc="SidakSD")


###################################################
### chunk number 8: pg 278
###################################################
library(biology)
Mbargraph(sanchez$GUANO, sanchez$COLTYPE, symbols=c('A','B','A'), ylab="Mean percentage Guano cover", xlab="Bird colony type")

