###################################################
### chunk number 1: pg 419
###################################################
library(nlme)
# model with random intercept and slope
spf.lme.1<-lme(Y~A*C, random=~C|B, spf)
# model without random slope
spf.lme.2 <- update(spf.lme.1, random=~1|B)
#Compare fitted models
#technically,only models fitted with ML (not REML)
#should be compared via anova
spf.lmeML.1 <- update(spf.lme.1, method="ML")
spf.lmeML.2 <- update(spf.lme.2, method="ML")
anova(spf.lmeML.1,spf.lmeML.2)


###################################################
### chunk number 2: pg 419
###################################################
# incorporate a first order autoregressive correlation structure 
# into the model without a random slope
spf.lme.3 <- update(spf.lme.2, correlation=corAR1(form=~1|B))
spf.lmeML.3 <- update(spf.lme.3, method="ML")
anova(spf.lmeML.2,spf.lmeML.3)
spf.lme.4 <- update(spf.lme.2, correlation=corCompSymm(form=~1|B))
spf.lmeML.4 <- update(spf.lmeML.4, method="ML")
anova(spf.lmeML.2,spf.lmeML.4)
spf.lme.5 <- update(spf.lme.2, correlation=corSymm(form=~1|B))
spf.lmeML.5 <- update(spf.lmeML.5, method="ML")
anova(spf.lmeML.2,spf.lmeML.5)


###################################################
### chunk number 3: pg 419
###################################################
# examine the fixed effects
anova(spf.lme.2)


###################################################
### chunk number 4: pg 420
###################################################
library(biology)
anova(mainEffects(spf.lme.2, at=A=="A1"))


###################################################
### chunk number 5: pg 420
###################################################
anova(mainEffects(spf.lme.2,at=A=="A2"))


###################################################
### chunk number 6: pg 420
###################################################
anova(mainEffects(spf.lme.2,at=C=="C1"))


###################################################
### chunk number 7: pg 420
###################################################
anova(mainEffects(spf.lme.2,at=C=="C2"))


###################################################
### chunk number 8: pg 420
###################################################
library(lme4)
spf.lmer <- lmer(Y~A*C+(1|B),spf)
library(languageR)
aovlmer.fnc(spf.lmer, noMCMC=T)
