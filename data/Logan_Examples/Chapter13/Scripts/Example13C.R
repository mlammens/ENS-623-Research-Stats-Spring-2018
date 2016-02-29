###################################################
### chunk number 1: pg 388
###################################################
driscoll1 <- driscoll
driscoll1[9,4] <- NA


###################################################
### chunk number 2: pg 388
###################################################
driscoll1.aov <- aov(CALLS~Error(BLOCK)+YEAR, data=driscoll1, subset=BLOCK!="newpipe")
summary(driscoll1.aov)


###################################################
### chunk number 3: pg 389
###################################################
#calculate the mean of the newpipe block
BM<-with(driscoll1, tapply(CALLS, BLOCK, mean, na.rm=T))["newpipe"]
#calculate the mean of year 2
YM<-with(driscoll1, tapply(CALLS, YEAR, mean, na.rm=T))["2"]
#calculate the overall mean
M<-mean(driscoll1$CALLS,na.rm=T)
#duplicate the data set and work on the duplicate
driscoll2 <- driscoll1
#substitute the new value into the data frame
driscoll2[9,3]<-YM+BM-M
#fit the linear model
driscoll2.aov <- aov(CALLS~Error(BLOCK)+YEAR, data=driscoll2)
summary(driscoll2.aov)
#then make adjustments to the F-ratio and Pvalue (to reflect a reduction) 
#in residual degrees of freedom by one for each substituted value)
(MSresid <- summary(driscoll2.aov)[[2]][[1]]["Residuals","Sum Sq"]/9)
(Fyear <- summary(driscoll2.aov)[[2]][[1]]["YEAR","Mean Sq"]/MSresid)
(Pvalue <- 1-pf(Fyear, 2,8))


###################################################
### chunk number 4: pg 389
###################################################
#fit full model
driscoll1.aovF <- aov(CALLS~BLOCK+YEAR, data=driscoll1)
#fit reduced model
driscoll1.aovR <- aov(CALLS~BLOCK, data=driscoll1)
anova(driscoll1.aovF,driscoll1.aovR)


###################################################
### chunk number 5: pg 390
###################################################
anova(driscoll1.aovF)


###################################################
### chunk number 6: pg 390
###################################################
library(nlme)
#No structure
driscoll1.lme1 <- lme(CALLS ~ YEAR, random = ~1 | BLOCK, data = driscoll1, subset = !is.na(CALLS))
#Unstructured 
driscoll1.lme2 <- lme(CALLS ~ YEAR, random = ~1 | BLOCK, data = driscoll1, subset = !is.na(CALLS), correlation = corSymm(form = ~1 | BLOCK))
#Compound symmetry
driscoll1.lme3 <- update(driscoll1.lme1, correlation = corCompSymm(form = ~1 | BLOCK))
#First order autoregressive
driscoll1.lme4 <- lme(CALLS ~ YEAR, random = ~1 | BLOCK, data = driscoll1, subset = !is.na(CALLS), correlation = corAR1(form = ~1 | BLOCK))
driscoll1.lme4 <- update(driscoll1.lme1, correlation = corAR1(form = ~1 | BLOCK))
#Compare each to compound symmetry
#technically,only models fitted with ML (not REML)
#should be compared via anova
driscoll1.lmeML1<-update(driscoll1.lme1, method="ML")
driscoll1.lmeML2<-update(driscoll1.lme2, method="ML")
driscoll1.lmeML3<-update(driscoll1.lme3, method="ML")
driscoll1.lmeML4<-update(driscoll1.lme4, method="ML")
anova(driscoll1.lmeML3, driscoll1.lmeML1, driscoll1.lmeML2, driscoll1.lmeML4)
anova(driscoll1.lme3)
