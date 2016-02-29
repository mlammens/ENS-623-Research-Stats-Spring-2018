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
### chunk number 102: McgoldLMEAnova
###################################################
anova(mcgold.lme.2)


###################################################
### chunk number 103: McGoldLMEMainEffectsIronbark
###################################################
library(biology)
anova(mainEffects(mcgold.lme.2, at=HABITAT=="ironbark"))


###################################################
### chunk number 104: McGoldLMEMainEffectsStringybark
###################################################
anova(mainEffects(mcgold.lme.2, at=HABITAT=="stringybark"))


###################################################
### chunk number 105: Alterwidth
###################################################
op <- options(width=200)


###################################################
### chunk number 106: McGoldPoly
###################################################
op <- options(width=200)
summary(mcgold.lme.2)$tTable


###################################################
### chunk number 107: Alterwidth1
###################################################
options(op)


###################################################
### chunk number 108: Alterwidth
###################################################
op <- options(width=200)


###################################################
### chunk number 109: McGoldPolyMainEffectsIronbark
###################################################
summary(mainEffects(mcgold.lme.2, at=HABITAT=="ironbark"))$tTable


###################################################
### chunk number 110: Alterwidth1
###################################################
options(op)


###################################################
### chunk number 111: Alterwidth
###################################################
op <- options(width=200)


###################################################
### chunk number 112: McGoldPolyMainEffectsStringybark
###################################################
summary(mainEffects(mcgold.lme.2, at=HABITAT=="stringybark"))$tTable


###################################################
### chunk number 113: Alterwidth1
###################################################
options(op)


###################################################
### chunk number 114: CreateRFile
###################################################
Stangle("../../splitPlot.rnw")


