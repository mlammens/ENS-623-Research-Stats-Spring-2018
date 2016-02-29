###################################################
### chunk number 1: pg 443
###################################################
library(nlme)
# fit a model without correlation structure
mcgold.lme.1 <- lme(LBIRDS ~ HABITAT * REGION * MONTH, random=~1 | 
SITE, data=mcgold)
# fit a model with a first order autoregressive correlation structure
mcgold.lme.2 <- lme(LBIRDS ~ HABITAT * REGION * MONTH, random = ~1 | SITE, data = mcgold, correlation = corAR1(form = ~1 | SITE))
# compare the fit of models
#technically,only models fitted with ML (not REML)
#should be compared via anova
mcgold.lmeML.1 <- update(mcgold.lme.1, method="ML")
mcgold.lmeML.2 <- update(mcgold.lme.2, method="ML")
anova(mcgold.lmeML.1, mcgold.lmeML.2)
# fit a model with compound symmetry structure
mcgold.lme.3 <- update(mcgold.lme.1, correlation = corCompSymm(form = ~1 | SITE))
mcgold.lmeML.3 <- update(mcgold.lme.3, method="ML")
anova(mcgold.lmeML.2, mcgold.lmeML.3)


###################################################
### chunk number 2: pg 443
###################################################
anova(mcgold.lme.2)


###################################################
### chunk number 3: pg 443
###################################################
library(biology)
anova(mainEffects(mcgold.lme.2, at=HABITAT=="ironbark"))


###################################################
### chunk number 4: pg 443
###################################################
op <- options(width=200)
anova(mainEffects(mcgold.lme.2, at=HABITAT=="stringybark"))


###################################################
### chunk number 5: pg 444
###################################################
summary(mcgold.lme.2)$tTable


###################################################
### chunk number 6: pg 445
###################################################
summary(mainEffects(mcgold.lme.2, at=HABITAT=="ironbark"))$tTable


###################################################
### chunk number 7: pg 446
###################################################
summary(mainEffects(mcgold.lme.2, at=HABITAT=="stringybark"))$tTable
#restore device output width
options(op)
