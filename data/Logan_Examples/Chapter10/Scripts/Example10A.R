###################################################
### chunk number 1: pg 265
###################################################
medley <- read.table('medley.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 266
###################################################
medley$ZINC <- factor(medley$ZINC, levels=c('HIGH', 'MED', 'LOW', 'BACK'), ordered=F)


###################################################
### chunk number 3: pg 266
###################################################
boxplot(DIVERSITY~ZINC, medley)


###################################################
### chunk number 4: pg 266
###################################################
plot(tapply(medley$DIVERSITY, medley$ZINC, mean),
tapply(medley$DIVERSITY, medley$ZINC, var))


###################################################
### chunk number 5: pg 266
###################################################
medley.aov <- aov(DIVERSITY~ZINC, medley)
plot(medley.aov)


###################################################
### chunk number 6: pg 267
###################################################
anova(medley.aov)


###################################################
### chunk number 7: pg 267
###################################################
library(multcomp)
summary(glht(medley.aov, linfct=mcp(ZINC='Tukey')))


###################################################
### chunk number 8: pg 268
###################################################
library(biology)
Mbargraph(medley$DIVERSITY, medley$ZINC,symbols=c('A','AB','B','AB'), ylab="Mean diatom diversity", xlab="Zinc concentration")
