###################################################
### chunk number 1: pg 279
###################################################
ants <- read.table('ants.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 279
###################################################
boxplot(BIOMASS~MONTH, ants)


###################################################
### chunk number 3: pg 279
###################################################
boxplot(BIOMASS^(1/3)~MONTH, ants)


###################################################
### chunk number 4: pg 280
###################################################
stat <- function(data, indices) {
f.ratio <- anova(aov(BIOMASS^(1/3)~MONTH, data))$"F value"[1]
f.ratio
}


###################################################
### chunk number 5: pg 280
###################################################
rand.gen <- function(data,mle) {
out <- data
out$MONTH <- sample(out$MONTH, replace=F)
out
}


###################################################
### chunk number 6: pg 280
###################################################
library(boot)
ants.boot <- boot(ants, stat, R=5000, sim="parametric", ran.gen=rand.gen)


###################################################
### chunk number 7: pg 280
###################################################
plot(ants.boot)


###################################################
### chunk number 8: pg 280
###################################################
print(ants.boot)


###################################################
### chunk number 9: pg 281
###################################################
f <- length(ants.boot[ants.boot$t >= ants.boot$t0])+1
print(f/(ants.boot$R + 1))


###################################################
### chunk number 10: pg 281
###################################################
#compare each of the months pairwise
ants.rand1 <- boot(ants[ants$MONTH=='September' | ants$MONTH=='August',], stat, R=1000, sim="parametric", ran.gen=rand.gen)
ants.rand2 <- boot(ants[ants$MONTH=='September' | ants$MONTH=='July',], stat, R=1000, sim="parametric", ran.gen=rand.gen)
ants.rand3 <- boot(ants[ants$MONTH=='September' | ants$MONTH=='June',], stat, R=1000, sim="parametric", ran.gen=rand.gen)
p.S.Jn <-print(length(ants.rand3[ants.rand3$t >= ants.rand3$t0])/(ants.rand3$R+ 1))
ants.rand4 <- boot(ants[ants$MONTH=='August' | ants$MONTH=='July',], stat, R=1000, sim="parametric", ran.gen=rand.gen)
p.A.Jy <-print(length(ants.rand4[ants.rand4$t >= ants.rand4$t0])/(ants.rand4$R+ 1))
ants.rand5 <- boot(ants[ants$MONTH=='August' | ants$MONTH=='June',], stat, R=1000, sim="parametric", ran.gen=rand.gen)
p.A.Jn <-print(length(ants.rand5[ants.rand5$t >= ants.rand5$t0])/(ants.rand5$R+ 1))
ants.rand6 <- boot(ants[ants$MONTH=='August' | ants$MONTH=='June',], stat, R=1000, sim="parametric", ran.gen=rand.gen)
p.Jy.Jn <-print(length(ants.rand6[ants.rand6$t >= ants.rand6$t0])/(ants.rand6$R+ 1))



###################################################
### chunk number 11: pg 281
###################################################
p.values <- c('Sep vs Aug'=p.S.A, 'Sep vs Jul'=p.S.Jy, 'Sep vs Jun'=p.S.Jn, 'Aug vs Jul'=p.A.Jy, 'Aug vs Jun'=p.A.Jn, 'Jul vs Jun'=p.Jy.Jn)
p.adjust(p.values,'holm')


###################################################
### chunk number 12: pg 282
###################################################
Mbargraph(ants$BIOMASS, ants$MONTH, symbols=c('A','AB','AB','B'), ylab="Mean ant biomass", xlab="Month")
