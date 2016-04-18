## Lecture 9 - In class

# The relation of the distribution of C3 plants in space - measured as lat and lon
c3_plants <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter9/Data/paruelo.csv")

# Look at the data
summary(c3_plants)

## Have a look
library(ggplot2)
library(GGally)

ggpairs(c3_plants)

## Center our lat/lon
c3_plants$LAT <- scale(c3_plants$LAT, scale = FALSE)
c3_plants$LONG <- scale(c3_plants$LONG, scale = FALSE)

## Build a linear model with no interaction
c3_plants_lm_noint <- lm(data = c3_plants, log10(C3 + 0.1) ~ LONG + LAT)
summary(c3_plants_lm_noint)

## Build a linear model with interaction term
c3_plants_lm_int <- lm(data = c3_plants, log10(C3 + 0.1) ~ LONG * LAT)
c3_plants_lm_int <- lm(data = c3_plants, log10(C3 + 0.1) ~ LONG + LAT + LONG:LAT)

summary(c3_plants_lm_int)

## Look at diagnostic plots
plot(c3_plants_lm_int)

## Looking at effect of lat on longitude to C3 relationships
## by looking at slopes at non-mean values

## Step away by two SD
LAT_sd1 <- mean(c3_plants$LAT) - 2*sd(c3_plants$LAT)
c3_plants_LONG.lm1 <- lm(log10(C3 + 0.1) ~ LONG * c(LAT-LAT_sd1), data = c3_plants)
summary(c3_plants_LONG.lm1)

## Go in the other direction
LAT_sd2 <- mean(c3_plants$LAT) + 2*sd(c3_plants$LAT)
c3_plants_LONG.lm2 <- lm(log10(C3 + 0.1) ~ LONG * c(LAT-LAT_sd2), data = c3_plants)
summary(c3_plants_LONG.lm2)

## Model selection
anova(c3_plants_lm_int, c3_plants_lm_noint)

## ************************************************************************** ##

# Some times your trend just isn't linear

blue_mussel <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter9/Data/mytilus.csv")

## Frequency of lap^94 versus in blue mussel versus distance from Southampton
summary(blue_mussel)

# Arcsine transform
blue_mussel$asinLAP <- asin( sqrt(blue_mussel$LAP)) * 180/pi
## See Logan p. 69 for table of transformations

qplot(blue_mussel$LAP)
qplot(blue_mussel$asinLAP)

# Lets look at relationship between lap and distance
ggplot(data = blue_mussel, aes(x = DIST, y = asinLAP)) +
  geom_point() +
  theme_bw() +
  stat_smooth()

## What would a linear model look like?
ggplot(data = blue_mussel, aes(x = DIST, y = asinLAP)) +
  geom_point() +
  theme_bw() +
  stat_smooth(method = "lm")

## Build model and look at it
blue_mussel_lm <- lm(data = blue_mussel, asinLAP ~ DIST)
summary(blue_mussel_lm)

## Look at diagnostics
plot(blue_mussel_lm)

## Make the line bend
blue_mussel_lm2 <- lm(data = blue_mussel, asinLAP ~ DIST + I(DIST^2))
summary(blue_mussel_lm2)

## Visualize this model
ggplot(data = blue_mussel, aes(x = DIST, y = asinLAP)) +
  geom_point() +
  theme_bw() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2) )

plot(blue_mussel_lm2)
anova(blue_mussel_lm2, blue_mussel_lm)

## Make the line bend MORE
blue_mussel_lm3 <- lm(data = blue_mussel, asinLAP ~ DIST + I(DIST^2) + I(DIST^3))
summary(blue_mussel_lm3)

## Visualize this model
ggplot(data = blue_mussel, aes(x = DIST, y = asinLAP)) +
  geom_point() +
  theme_bw() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) )

## AIC
## measure of deviance, penalized by number of parameters
## AIC is meaningless by itself
AIC(blue_mussel_lm)
AIC(blue_mussel_lm2)
AIC(blue_mussel_lm3)
AIC(lm(data = blue_mussel, formula = asinLAP ~ DIST + I(DIST^2) + I(DIST^3) + I(DIST^4)))



# End








sar <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter9/Data/peake.csv")
