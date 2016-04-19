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


## Non-linear regression

## Power-law example in Ecology - species area curve
## Number of species = constant * Area^z

## Number of invert species living in inter-tidal mussel clumps
sar <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter9/Data/peake.csv")

## Look at these data
head(sar)
summary(sar)

## Plot these data
ggplot(data = sar, aes(x = AREA, y = SPECIES)) +
  geom_point() + 
  stat_smooth() +
  theme_bw()

## Fit a linear model
sar_lm <- lm(data = sar, SPECIES ~ AREA)
summary(sar_lm)
plot(sar_lm)

## Fit a non-linear model
?nls
sar_nls <- nls(data = sar, formula = SPECIES ~ const * AREA^z,
               start = list(const = 0.1, z = 1) )
summary(sar_nls)


## Build an ANOVA
## Diversity of diatoms based on the impact of ZINC. Diatoms that were found in RMNP.

diatoms <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter10/Data/medley.csv")

head(diatoms)

## Plot these data
ggplot(data = diatoms, aes(x = ZINC, y = DIVERSITY)) +
  geom_point() + 
  theme_bw()

ggplot(data = diatoms, aes(x = ZINC, y = DIVERSITY)) +
  geom_boxplot() + 
  theme_bw()

## Running an ANOVA
diatoms_lm <- lm(data = diatoms, DIVERSITY ~ ZINC)
summary(diatoms_lm)

anova(diatoms_lm)


## Second way to do ANOVA
summary(aov(data = diatoms, DIVERSITY ~ ZINC))
