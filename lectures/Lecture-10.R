# Lecture 10 Notes

buckthorn <- read.csv(file = "https://mlammens.github.io/Biostats/data/buckthorn.csv")

head(buckthorn)

library(ggplot2)

ggplot(data = buckthorn, aes(x = Years, y = OccGrids) ) +
  geom_point() +
  stat_smooth() +
  stat_smooth( method = "lm", colour = "red") +
  theme_bw()
  
## Fit a simple model
summary(lm(data = buckthorn, formula = OccGrids ~ Years))
buckthorn_lm <- lm(data = buckthorn, formula = sqrt(OccGrids) ~ Years)
plot(lm(data = buckthorn, formula = OccGrids ~ Years))


## Take the square root of y
ggplot(data = buckthorn, aes(x = Years, y = sqrt(OccGrids)) ) +
  geom_point() +
  stat_smooth() +
  #stat_smooth( method = "lm", colour = "red") +
  theme_bw()


buckthorn_lm2 <- lm(data = buckthorn, formula = sqrt(OccGrids) ~ Years + I(Years^2))
summary(buckthorn_lm2)

plot(buckthorn_lm2)

anova(buckthorn_lm2, buckthorn_lm)

buckthorn_lm3 <- lm(data = buckthorn, formula = sqrt(OccGrids) ~ Years + I(Years^2) +I(Years^3))

ggplot(data = buckthorn, aes(x = Years, y = sqrt(OccGrids)) ) +
  geom_point() +
  stat_smooth( method = "lm", formula = y ~ x + I(x^2), colour = "blue") +
  stat_smooth( method = "lm", formula = y ~ x + I(x^2) + I(x^3), colour = "red") +
  theme_bw()



anova(buckthorn_lm3, buckthorn_lm2)

AIC(buckthorn_lm3)
AIC(buckthorn_lm2)
