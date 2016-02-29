###################################################
### chunk number 1: pg 430
###################################################
library(lattice)
mullens$O2 <- as.numeric(as.character(mullens$O2LEVEL))
xyplot(SFREQBUC~O2|TOAD, groups=BRTH.TYP,mullens, type=c("p","r"), auto.key=T)


###################################################
### chunk number 2: pg 430
###################################################
library(nlme)
# model with random intercept and slope
contrasts(mullens$BRTH.TYP) <- "contr.helmert"
contrasts(mullens$O2LEVEL) <- contr.poly(8, c(0, 5, 10, 15, 20, 30, 40, 50))
# fit a model without correlation structure
mullens.lme.1 <- lme(SFREQBUC ~ BRTH.TYP * O2LEVEL, random = ~1 | TOAD,
data = mullens)
# fit a model with a compound symmetry correlation structure
mullens.lme.2 <- lme(SFREQBUC ~ BRTH.TYP * O2LEVEL, random = ~1 | TOAD,
data = mullens, corr = corCompSymm(form = ~1 | TOAD))
# compare the fit of models
#technically,only models fitted with ML (not REML)
#should be compared via anova
mullens.lmeML.1 <- update(mullens.lme.1, method="ML")
mullens.lmeML.2 <- update(mullens.lme.2, method="ML")
anova(mullens.lmeML.1, mullensML.lme.2)


###################################################
### chunk number 2: pg 431
###################################################
mullens.lme.3 <- lme(SFREQBUC ~ BRTH.TYP * O2LEVEL, random = ~1 | TOAD, data = mullens, corr = corAR1(form = ~1 | TOAD))
# compare the fit of models
mullens.lmeML.3 <- update(mullens.lme.3, method="ML")
anova(mullens.lmeML.1, mullens.lmeML.3)


###################################################
### chunk number 3: pg 431
###################################################
anova(mullens.lme.3, type="marginal")


###################################################
### chunk number 4: pg 431
###################################################
library(biology)
anova(mainEffects(mullens.lme.3, at=BRTH.TYP=="buccal"))


###################################################
### chunk number 5: pg 432
###################################################
anova(mainEffects(mullens.lme.3, at=BRTH.TYP=="lung"))


###################################################
### chunk number 6: pg 432
###################################################
summary(mullens.lme.3)$tTable


###################################################
### chunk number 7: pg 432
###################################################
summary(mainEffects(mullens.lme.3, at=BRTH.TYP=="buccal"))$tTable


###################################################
### chunk number 8: pg 433
###################################################
summary(mainEffects(mullens.lme.3, at=BRTH.TYP=="lung"))$tTable

