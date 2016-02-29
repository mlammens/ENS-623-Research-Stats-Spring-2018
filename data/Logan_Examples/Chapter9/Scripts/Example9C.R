###################################################
### chunk number 1: pg 238
###################################################
library(biology)
m<-Model.selection(loyn.lm)
Model.selection(loyn.lm)[[1]][1:45,c(2,5,6,7)]


###################################################
### chunk number 2: pg 238
###################################################
library(MuMIn)
model.avg(get.models(dredge(loyn.lm, rank="AIC")))


###################################################
### chunk number 3: pg 239
###################################################
loyn.lm2<-lm(ABUND~log10(AREA)+GRAZE, data=loyn)
summary(loyn.lm2)

