###################################################
### chunk number 1: pg 510
###################################################
gotelli <- read.table('gotelli.csv', header=T, sep=',')


###################################################
### chunk number 2: pg 510
###################################################
gotelli$cLatitude<-scale(gotelli$Latitude,scale=F)
gotelli$cElevation<-scale(gotelli$Elevation,scale=F)


###################################################
### chunk number 3: pg 511
###################################################
library(car)
scatterplot.matrix(~Srich+Habitat*cLatitude*cElevation, data=gotelli)


###################################################
### chunk number 4: pg 511
###################################################
gotelli.glm <- glm(Srich~Habitat*cLatitude*cElevation, family=poisson,data=gotelli)
vif(gotelli.glm)


###################################################
### chunk number 5: pg 512
###################################################
pp<-sum(resid(gotelli.glm,type='pearson')^2)
1-pchisq(pp,gotelli.glm$df.resid)


###################################################
### chunk number 6: pg 512
###################################################
1-pchisq(gotelli.glm$deviance, gotelli.glm$df.resid)


###################################################
### chunk number 7: pg 512
###################################################
influence.measures(gotelli.glm)


###################################################
### chunk number 8: pg 512
###################################################
#via Pearson residuals
pp/gotelli.glm$df.resid
#OR via deviance
gotelli.glm$deviance/gotelli.glm$df.resid


###################################################
### chunk number 9: pg 512
###################################################
summary(gotelli.glm)


###################################################
### chunk number 10: pg 513
###################################################
library(MuMIn)
model.avg(get.models(dredge(gotelli.glm)))


###################################################
### chunk number 11: pg 514
###################################################
gotelli.glm <- glm(Srich~Habitat+Latitude+Elevation, family=poisson,data=gotelli)
summary(gotelli.glm)


###################################################
### chunk number 12: pg 515
###################################################
# Produce base plot
xs <- seq(40, 45, l = 1000)
plot(Srich ~ Latitude, data = gotelli, type = "n", axes = F, xlab = "", ylab = "")
# Plot the points and predicted trends
points(Srich ~ Latitude, data = gotelli, subset = Habitat == "Forest", pch = 16)
pred <- predict(gotelli.glm, type = "response", se = T, newdata = data.frame(Latitude = xs, Habitat = "Forest", Elevation = mean(gotelli$Elevation)))
lines(pred$fit ~ xs)
points(Srich ~ Latitude, data = gotelli, subset = Habitat == "Bog", pch = 21)
pred <- predict(gotelli.glm, type = "response", se = T, newdata = data.frame(Latitude = xs, Habitat = "Bog", Elevation = mean(gotelli$Elevation)))
lines(pred$fit ~ xs)
# Axes titles
mtext("Ant species richness", 2, line = 3)
axis(2, las=1)
mtext(expression(paste("Latitude (", degree*N, ")")), 1, line = 3)
axis(1)
legend("topright", legend = c("Forest", "Bog"), pch = c(16, 21), title = "Habitat", bty = "n")
box(bty = "l")
