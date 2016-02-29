###################################################
### chunk number 1: pg 525
###################################################
loyn <- read.table('loyn.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 525
###################################################
scatterplot.matrix(~ ABUND + AREA + I(1987-YR.ISOL) + DIST, data=loyn, diag="boxplot")


###################################################
### chunk number 3: pg 526
###################################################
library(car)
vif(glm(ABUND~log10(AREA)+I(1987-YR.ISOL)+log10(DIST), family=gaussian,data=loyn))


###################################################
### chunk number 4: pg 526
###################################################
library(gam)
loyn.gam<-gam(ABUND~lo(log10(AREA))+lo(I(1987-YR.ISOL))+lo(log10(DIST)), family=gaussian,data=loyn)


###################################################
### chunk number 5: pg 526
###################################################
paste("Deviance:",format(loyn.gam$deviance))
1-pchisq(loyn.gam$deviance, loyn.gam$df.resid)


###################################################
### chunk number 6: pg 526
###################################################
# extract the Pearson's residuals from the fitted gam
loyn.Res <- residuals(loyn.gam, "pearson")
#generate a data frame with all transformations
loyn.mod <- with(loyn, data.frame(ABUND, L10AREA = log10(AREA), YRSISOL = I(1987 - YR.ISOL), L10DIST = log10(DIST)))
# rearrange this data frame such that each of the predictors become 
# levels of a factor vector
loyn.L <- reshape(loyn.mod, direction = "long", varying = list(c(2, 3, 4)), timevar = "Predictor", v.names = "Var", times = names(loyn.mod[, c(2, 3, 4)]))
# add the residuals to this data frame
loyn.L$Res <- rep(loyn.Res, 3)
# construct a lattice graphic
library(lattice)
print(xyplot(Res ~ Var | Predictor, data = loyn.L, scales = list(
alternating = TRUE, x = list(relation = "free")), xlab=
"Predictor variables", panel = function(x,y) {
 panel.points(x, y, col = 1, pch = 16)
 panel.loess(x, y, lwd = 2, col = 1)
 }
 ))


###################################################
### chunk number 7: 528
###################################################
summary(loyn.gam)


###################################################
### chunk number 8: pg 528
###################################################
loyn.gam1<-update(loyn.gam, ~.-lo(log10(AREA)), family=gaussian,data=loyn)
anova(loyn.gam, loyn.gam1, test="F")


###################################################
### chunk number 9: pg 529
###################################################
loyn.gam2<-update(loyn.gam, ~.-lo(I(1987 - YR.ISOL)), family=gaussian,data=loyn)
anova(loyn.gam, loyn.gam2, test="F")


###################################################
### chunk number 10: pg 529
###################################################
loyn.gam3<-update(loyn.gam, ~.-lo(log10(DIST)), family=gaussian,data=loyn)
anova(loyn.gam, loyn.gam3, test="F")


###################################################
### chunk number 11: pg 529
###################################################
library(MuMIn)
dredge(loyn.gam)
