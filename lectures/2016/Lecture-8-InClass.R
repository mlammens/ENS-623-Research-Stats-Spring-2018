## Lecture 8 - In Class Notes

### Same example as in Lecture 7

flr_beetle <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter8/Data/nelson.csv")
flr_beetle

## Plot these data
library(ggplot2)

ggplot() +
  geom_point(data = flr_beetle, aes(x = HUMIDITY, y = WEIGHTLOSS)) +
  theme_bw()

## Fit a linear model
flr_beetle_lm <- lm(data = flr_beetle, WEIGHTLOSS ~ HUMIDITY)

summary(flr_beetle_lm)

## Let's plot the linear model on top of our data
ggplot(data = flr_beetle, aes(x = HUMIDITY, y = WEIGHTLOSS)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()

ggplot(data = flr_beetle, aes(x = HUMIDITY, y = WEIGHTLOSS)) +
  geom_point() +
  stat_smooth() +
  theme_bw()

## Getting the residuals
flr_beetle_lm$residuals

## Diagnostic plots
plot(flr_beetle_lm)

## LOESS Fit for Iris data
ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point() +
  theme_bw() +
  stat_smooth() +
  stat_smooth(aes(colour = Species)) +
  stat_smooth(method = "lm", colour = "red")

## Multiple regression example

## the effects of habitat fragmentation on abundance for forest birds
bird_frag <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter9/Data/loyn.csv")
summary(bird_frag)

#install.packages("GGally")
library(GGally)

ggpairs(bird_frag)

## Transform some of these data
bird_frag_transform <- bird_frag

bird_frag_transform$AREA <- log10(bird_frag_transform$AREA)
bird_frag_transform$DIST <- log10(bird_frag_transform$DIST)
bird_frag_transform$LDIST <- log10(bird_frag_transform$LDIST)

## Replot
ggpairs(bird_frag_transform)

library(car)

vif(lm(data = bird_frag, ABUND ~ log10(AREA) + YR.ISOL + log10(DIST) + log10(LDIST) + GRAZE + ALT))

## Make our linear model
bird_frag_lm <- lm(data = bird_frag, ABUND ~ log10(AREA) + YR.ISOL + log10(DIST) + log10(LDIST) + GRAZE + ALT)

## Summary of our model
summary(bird_frag_lm)

## Look at diagnostic plots
plot(bird_frag_lm)

avPlots(bird_frag_lm, ask = FALSE)
