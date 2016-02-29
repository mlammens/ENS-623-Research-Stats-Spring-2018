###################################################
### chunk number 1: pg 380
###################################################
driscoll <- read.table('driscoll.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 380
###################################################
driscoll$YEAR <-factor(driscoll$YEAR)


###################################################
### chunk number 2: pg 380
###################################################
boxplot(CALLS~YEAR, driscoll)


###################################################
### chunk number 3: pg 380
###################################################
library(alr3)
resplot(lm(CALLS~BLOCK+YEAR, driscoll))


###################################################
### chunk number 4: pg 380
###################################################
with(driscoll, interaction.plot(BLOCK, YEAR, CALLS))


###################################################
### chunk number 5: pg 381
###################################################
replications(CALLS~Error(BLOCK)+YEAR, data=driscoll)
library(biology)
is.balanced(CALLS~Error(BLOCK)+YEAR, data=driscoll)


###################################################
### chunk number 6: pg 381
###################################################
driscoll.aov <- aov(CALLS~Error(BLOCK)+YEAR, data=driscoll)


###################################################
### chunk number 7: pg 381
###################################################
library(biology)
AnovaM(driscoll.aov, RM=T)


###################################################
### chunk number 8: pg 382
###################################################
#convert the data to wide format
dris.rm <- reshape(driscoll, timevar = "YEAR", v.names = "CALLS", idvar = "BLOCK", direction = "wide")
#fit the simple MANOVA
dris.lm <- lm(cbind(CALLS.1, CALLS.2, CALLS.3) ~ 1, dris.rm)
#create a data frame that defines the intra-block design
idata <- data.frame(YEAR = as.factor(c(1, 2, 3)))
#use the Anova (car) function to estimate the MANOVA test statistics
(av.ok <- Anova(dris.lm, idata = idata, idesign = ~YEAR))
summary(av.ok)     

###################################################
### chunk number 9: pg 384
###################################################
driscoll.aov2 <- aov(CALLS ~ C(YEAR,c(1,-.5,-.5), 1) + BLOCK + Error(BLOCK/C(YEAR, c(1,-.5,-.5), 1)), data=driscoll)
summary(driscoll.aov2)


###################################################
### chunk number 10: pg 385
###################################################
driscoll.aov3 <- aov(CALLS ~ C(YEAR,poly, 1) + BLOCK + Error(BLOCK/C(YEAR, poly, 1)), data=driscoll)
summary(driscoll.aov3)


###################################################
### chunk number 11: pg 386
###################################################
library(nlme)
#fit the lme with unstructured covariance structure
driscoll.lme <- lme(CALLS ~ YEAR, random =~1 | BLOCK, data = driscoll, correlation = corSymm(form = ~1 | BLOCK))
#fit the lme assuming compound symmetry (sphericity)
driscoll.lme1 <- update(driscoll.lme, correlation = corCompSymm(form = ~1 | BLOCK))
#compare the fit of the models
anova(driscoll.lme, driscoll.lme1)
#technically,only models fitted with ML (not REML)
#should be compared via anova
driscoll.lmeML1 <- update(driscoll.lme, correlation = corCompSymm(form = ~1 | BLOCK), method="ML")
driscoll.lmeML <- update(driscoll.lme, method="ML")
anova(driscoll.lmeML, driscoll.lmeML1)
#fit the lme with a first order autoregressive covariance structure
driscoll.lme2 <- update(driscoll.lme, correlation = corAR1(form = ~1 | BLOCK))
driscoll.lme2
driscoll.lmeML2 <- update(driscoll.lme, correlation = corAR1(form = ~1 | BLOCK), method="ML")


###################################################
### chunk number 12: pg 386 
###################################################
#compare the fit of the models
anova(driscoll.lmeML2, driscoll.lmeML1)
#fit the lme with a first order autoregressive covariance and heterogenous 
#variances structure
driscoll.lme3 <- update(driscoll.lme, correlation = corAR1(form = ~1 | BLOCK), weights = varIdent(form = ~1 | BLOCK))
driscoll.lmeML3 <- update(driscoll.lme, correlation = corAR1(form = ~1 | BLOCK), weights = varIdent(form = ~1 | BLOCK), method="ML")
#compare the fit of the models
anova(driscoll.lmeML3, driscoll.lmeML1)


###################################################
### chunk number 13: pg 387
###################################################
#final anova should be based on REML model
anova(driscoll.lme1)


###################################################
### chunk number 14: pg 387
###################################################
library(gmodels)
fit.contrast(driscoll.lme1, "YEAR", c(1,-.5,-.5))


###################################################
### chunk number 15: pg 387
###################################################
library(gmodels)
fit.contrast(driscoll.lme1, "YEAR", t(contr.poly(3, c(1,2,3))))


###################################################
### chunk number 16: pg 387
###################################################
#create a blocking variable (called BLCK) that represents the 
#order of data in rows
driscoll$BLCK <- as.numeric(factor(driscoll$BLOCK, levels = unique(driscoll$BLOCK)))
# construct the base plot with different point types for each treatment
plot(CALLS ~ BLCK, data = driscoll, type = "n", axes = F, xlab = "", ylab = "")
with(subset(driscoll, YEAR == "1"), points(CALLS ~ BLCK, pch = 21, type = "o", lwd = 1))
with(subset(driscoll, YEAR == "2"), points(CALLS ~ BLCK, pch = 15, type = "o", lwd = 1, lty = 2))
with(subset(driscoll, YEAR == "3"), points(CALLS ~ BLCK, pch = 5, type = "o", lwd = 1, lty = 5))
# create the axes and their labels
axis(1, cex.axis = 0.8)
mtext(text = "Block", side = 1, line = 3)
axis(2, cex.axis = 0.8, las = 1)
mtext(text = "Difference in calls (burnt - unburnt)", side = 2, line = 3)
# include a legend
legend("topright",leg = c("Year 1", "Year 2", "Year 3"), lty = 0, pch = c(21, 15, 5), bty = "n", cex=0.9)
box(bty="l")
