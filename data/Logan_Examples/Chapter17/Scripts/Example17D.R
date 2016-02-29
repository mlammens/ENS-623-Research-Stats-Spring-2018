###################################################
### chunk number 1: pg 516
###################################################
sinclair <- read.table('sinclair.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 516
###################################################
sinclair.glm <- glm(COUNT~SEX*MARROW*DEATH,family=poisson, data=sinclair)


###################################################
### chunk number 3: pg 516
###################################################
library(MuMIn)
dredge(sinclair.glm,rank="AIC")


###################################################
### chunk number 4: pg 517
###################################################
sinclair.glm <- glm(COUNT~SEX*MARROW*DEATH,family=poisson, data=sinclair)


###################################################
### chunk number 5: pg 517
###################################################
sinclair.glm1 <-update(sinclair.glm,~.-SEX:MARROW:DEATH,data=sinclair)
anova(sinclair.glm,sinclair.glm1,test="Chisq")


###################################################
### chunk number 6: pg 517
###################################################
sinclair.glm2 <- update(sinclair.glm1,~.-SEX:DEATH, data=sinclair)
anova(sinclair.glm1,sinclair.glm2,test="Chisq")


###################################################
### chunk number 7: pg 518
###################################################
sinclair.glm4 <- update(sinclair.glm1,~.-SEX:MARROW, data=sinclair)
anova(sinclair.glm1,sinclair.glm4,test="Chisq")


###################################################
### chunk number 8: pg 518
###################################################
sinclair.glm3 <- update(sinclair.glm1,~.-DEATH:MARROW, data=sinclair)
anova(sinclair.glm1,sinclair.glm3,test="Chisq")


###################################################
### chunk number 9: pg 518
###################################################
xtabs(resid(sinclair.glm1, type="pearson") ~ SEX + MARROW + DEATH, sinclair)


###################################################
### chunk number 10: pg 519
###################################################
# females
sinclair.glmR <- glm(COUNT ~ DEATH + MARROW, family = poisson, data = sinclair, subset = SEX == "FEMALE")
sinclair.glmF <- glm(COUNT ~ DEATH * MARROW, family = poisson, data = sinclair, subset = SEX == "FEMALE")
anova(sinclair.glmR, sinclair.glmF, test = "Chisq")


###################################################
### chunk number 11: pg 519
###################################################
# males
sinclair.glmR <- glm(COUNT ~ DEATH + MARROW, family = poisson, 
data = sinclair, subset = SEX == "MALE")
sinclair.glmF <- glm(COUNT ~ DEATH * MARROW, family = poisson, 
data = sinclair, subset = SEX == "MALE")
anova(sinclair.glmR, sinclair.glmF, test = "Chisq")


###################################################
### chunk number 12: pg 519
###################################################
# Males
library(biology)
male.tab <- xtabs(COUNT ~ DEATH + MARROW, data=sinclair, subset=SEX == "MALE")
# transpose to express in the context of cause of death
male.tab <- t(male.tab)
oddsratios(male.tab)


###################################################
### chunk number 13: pg 520
###################################################
# Females
female.tab <- xtabs(COUNT ~ DEATH + MARROW, data=sinclair, subset=SEX == "FEMALE")
female.tab <- t(female.tab)
oddsratios(female.tab)


###################################################
### chunk number 14: pg 520
###################################################
# make a table for females
female.tab <- xtabs(COUNT ~ DEATH + MARROW, data=sinclair, subset=SEX=="FEMALE")
# calculate the odds ratios for females
# the table should be transposed such that cause of death are in columns
library(biology)
sinclair.or<-oddsratios(t(female.tab))
plot(estimate~as.numeric(Comparison), data=sinclair.or, log="y", type="n", axes=F, xlab="",ylab="", ylim=range(c(upper,lower)), xlim=c(.5,3.5))
# plot the female data displaced to the right slightly
with(sinclair.or,points(as.numeric(Comparison)+.1, estimate,type="b"))
with(sinclair.or, arrows(as.numeric(Comparison)+.1, upper, as.numeric(Comparison)+.1,lower, ang=90, length=.1, code=3))
# make the male table
male.tab <- xtabs(COUNT ~ DEATH + MARROW, data=sinclair, subset=SEX=="MALE")
sinclair.or<-oddsratios(t(male.tab))
# plot the male odds ratios
points(estimate~Comparison,data=sinclair.or, type="b", pch=16)
with(sinclair.or, arrows(as.numeric(Comparison), upper, as.numeric(Comparison), lower, ang=90, length=.1, code=3))
abline(h=1,lty=2)
# put in axes and titles
with(sinclair.or,axis(1,at=as.numeric(Comparison), lab=Comparison))
axis(2,las=1, cex.axis=.75)
mtext("Marrow type",1, line=3)
mtext("Odds ratio of death by predation", 2, line=3)
legend("topright",legend=c("Male","Female"), pch=c(16,21), bty="n", title="Sex")
box(bty="l")
