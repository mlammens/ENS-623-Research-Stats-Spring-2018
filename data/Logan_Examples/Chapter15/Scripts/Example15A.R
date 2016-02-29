###################################################
### chunk number 1: pg 457
###################################################
partridge <- read.table('partridge.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 457
###################################################
plot(aov(LONGEV~THORAX + TREATMENT, partridge), which=1)


###################################################
### chunk number 3: pg 457
###################################################
plot(aov(log10(LONGEV) ~ THORAX + TREATMENT, partridge), which=1)


###################################################
### chunk number 4: pg 458
###################################################
library(lattice)
print(xyplot(log10(LONGEV)~THORAX|TREATMENT, partridge, type=c("r","p")))


###################################################
### chunk number 5: pg 458
###################################################
anova(aov(log10(LONGEV) ~ THORAX*TREATMENT, partridge))


###################################################
### chunk number 6: pg 459
###################################################
library(lattice)
print(with(partridge,xyplot(log10(LONGEV) ~ THORAX, groups=TREATMENT, type=c("p","r"), col=1, par.settings = list(superpose.symbol=list(pch=1:5,col=1), superpose.line=list(lty=1:6)), key=list(space ="right", lty=1:5, lines=T, points=T, pch=1:5,col=1, text=list(levels(TREATMENT))))))


###################################################
### chunk number 7: pg 459
###################################################
anova(aov(THORAX ~TREATMENT, partridge))


###################################################
### chunk number 8: pg 460
###################################################
# define contrasts
contrasts(partridge$TREATMENT) <- cbind(c(0, 0.5, 0.5, -0.5, -0.5), c(0, 0, 0, 1, -1))
# confirm that contrasts orthogonal
round(crossprod(contrasts(partridge$TREATMENT)), 1)


###################################################
### chunk number 9: pg 460
###################################################
partridge.aov <- aov(log10(LONGEV) ~ THORAX+TREATMENT, partridge)
library(biology)
AnovaM(partridge.aov, type="III", split=list(TREATMENT=list("Preg vs Virg"=1, "1 Virg vs 8 Virg"=2)))


###################################################
### chunk number 10: pg 461
###################################################
# create the base blank plot
plot(LONGEV ~ THORAX, partridge, type = "n", axes = F, xlab = "", ylab = "", log = "y")
xs <- seq(min(partridge$THORAX), max(partridge$THORAX), l = 1000)
# plot the None series
part.lm <- lm(LONGEV ~ THORAX, partridge, subset = TREATMENT == "None")
lines(xs, predict(part.lm, data.frame(THORAX = xs)), lty = 1)
points(LONGEV ~ THORAX, partridge, subset = TREATMENT == "None", type = "p", pch = 1)
# plot the Preg1 series
part.lm <- lm(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Preg1")
lines(xs, predict(part.lm, data.frame(THORAX = xs)), lty = 2)
points(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Preg1", type = "p", pch = 23, bg = "gray")
# plot the Preg8 series
part.lm <- lm(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Preg8")
lines(xs, predict(part.lm, data.frame(THORAX = xs)), lty = 3)
points(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Preg8", type = "p", pch = 24, bg = "gray")
# plot the Virg1 series
part.lm <- lm(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Virg1")
lines(xs, predict(part.lm, data.frame(THORAX = xs)), lty = 4)
points(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Virg1", type = "p", pch = 23, bg = "black")
# plot the Virg8 series
part.lm <- lm(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Virg8")
lines(xs, predict(part.lm, data.frame(THORAX = xs)), lty = 5)
points(LONGEV ~ THORAX, partridge, subset = TREATMENT == "Virg8", type = "p", pch = 24, bg = "black")
axis(1)
mtext("Thorax length (mm)", 1, line = 3)
axis(2, las = 1)
mtext(expression(paste("Male fruitfly longevity (days)")), 2, line = 3)
legend("bottomright", legend = c("None", "1 pregnant", "8 pregnant", "1virgin", "8 virgin"), bty = "n", title = "Treatment", lty = 1:6, pch = c(1, 23, 24, 23, 24), pt.bg = c(1, "gray", "gray", 1, 1))
box(bty = "l")
