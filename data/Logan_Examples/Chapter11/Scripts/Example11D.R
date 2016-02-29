###################################################
### chunk number 1: pg 307
###################################################
glyco <- read.table('glyco.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 307
###################################################
glyco$TREAT <- factor(glyco$TREAT, levels=c("Control","Compound217", "Compound217Sugar"))


###################################################
### chunk number 3: pg 308
###################################################
library(nlme)
glyco.treat.agg <- gsummary(glyco, groups=glyco$RAT)


###################################################
### chunk number 4: pg 308
###################################################
glyco.rat.agg <- gsummary(glyco, groups=glyco$PREP)


###################################################
### chunk number 5: pg 308
###################################################
glyco.prep.agg <- gsummary(glyco, groups=glyco$READ)


###################################################
### chunk number 6: pg 308
###################################################
library(biology)
is.balanced(GLYCO ~ TREAT + RAT + PREP, data=glyco)


###################################################
### chunk number 7: pg 308
###################################################
glyco.aov <- aov(GLYCO~TREAT + Error(RAT/PREP), glyco)
summary(glyco.aov)


###################################################
### chunk number 8: pg 309
###################################################
glyco.rat.aov <- aov(GLYCO~TREAT + RAT + Error(PREP), glyco.rat.agg)
summary(glyco.rat.aov)


###################################################
### chunk number 9: pg 309
###################################################
glyco.prep.aov <- aov(GLYCO~TREAT + RAT + PREP, glyco.prep.agg)
summary(glyco.prep.aov)


###################################################
### chunk number 10: pg 309
###################################################
library(nlme)
glyco.lme <- lme(GLYCO~TREAT,random=~1|RAT/PREP, glyco)
summary(glyco.lme)
anova(glyco.lme)


###################################################
### chunk number 11: pg 310
###################################################
library(nlme)
nlme:::VarCorr(glyco.lme)


###################################################
### chunk number 12: pg 311
###################################################
library(lme4)
glyco.lmer <- lmer(GLYCO~TREAT + (1|RAT/PREP), glyco)


###################################################
### chunk number 13: pg 311
###################################################
plot(resid(ph.lmer)~fitted(ph.lmer))


###################################################
### chunk number 14: pg 311
###################################################
glyco.lmer


###################################################
### chunk number 15: pg 312
###################################################
library(languageR)
glyco.pval<-pvals.fnc(glyco.lmer,nsim=10000, withMCMC=T)


###################################################
### chunk number 16: pg 312
###################################################
# Fixed effects
glyco.pval$fixed
# Random effects
glyco.pval$random


###################################################
### chunk number 17: pg 312
###################################################
glyco.mcmc <- glyco.pval$mcmc
library(biology)
mcmcpvalue(as.matrix(glyco.mcmc), "TREAT")
