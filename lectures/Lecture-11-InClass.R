# Notes from class 11

## Working with diatoms

## Bring in the data
diatoms <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter10/Data/medley.csv")

head(diatoms)

## Make linear model
diatoms_lm <- lm(data = diatoms, DIVERSITY ~ ZINC)

## Look at summary and anova table
summary(diatoms_lm)

anova(diatoms_lm)


## Make a box plot
library(ggplot2)

ggplot(data = diatoms) +
  geom_boxplot(aes(x = ZINC, y = DIVERSITY)) +
  theme_bw()


## Tukey post-hoc HSD comparison
TukeyHSD(aov(data = diatoms, DIVERSITY ~ ZINC))

table(diatoms$ZINC)
table(diatoms$STREAM)


## Sea urchins eating algae

algae_density <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter11/Data/andrew.csv")

head(algae_density)

## look at a summary
summary(algae_density)

## Convert Patch and Quad to factors
algae_density$PATCH <- factor(algae_density$PATCH)
algae_density$QUAD <- factor(algae_density$QUAD)

summary(algae_density)

algae_density$TREAT <- factor(algae_density$TREAT, levels = c("0%", "33%", "66%", "100%"))


## Visualize these data
library(ggplot2)

ggplot(data = algae_density) +
  geom_boxplot(aes(x = TREAT, y = ALGAE)) +
  theme_bw()

## Find patch means
library(dplyr)

algae_density_agg <-
  algae_density %>%
  group_by(TREAT, PATCH) %>%
  summarise(ALGAE_QUAD_MEAN = mean(ALGAE))

algae_density_agg

algae_density_agg$TREAT <- factor(algae_density_agg$TREAT, levels = c("0%", "33%", "66%", "100%"))

ggplot(data = algae_density_agg) +
  geom_boxplot(aes(x = TREAT, y = ALGAE_QUAD_MEAN)) +
  theme_bw()

## Run an anova with a nested factor
algae_density_aov <- aov(data = algae_density, ALGAE ~ TREAT + Error(PATCH))

summary(algae_density_aov)
summary(algae_density)

summary(aov(data = algae_density, ALGAE ~ TREAT))


## Factorial ANOVA
## 
## Two or more factors, not nested

## Limpets - does number of eggs depend on season and/or density
limpets <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter12/Data/quinn.csv")

head(limpets)
summary(limpets)

limpets$DENSITY <- factor(limpets$DENSITY)

## Get a visual
ggplot(data = limpets) +
  geom_boxplot(aes(x = DENSITY, y = EGGS)) +
  theme_bw()

ggplot(data = limpets) +
  geom_boxplot(aes(x = SEASON, y = EGGS)) +
  theme_bw()

ggplot(data = limpets) +
  geom_boxplot(aes(x = DENSITY, y = EGGS, fill = SEASON)) +
  theme_bw()

## Run ANOVA
limpets_lm <- lm(data = limpets, formula = EGGS ~ SEASON * DENSITY)
limpets_lm <- lm(data = limpets, formula = EGGS ~ SEASON + DENSITY + SEASON:DENSITY)

summary(limpets_lm)

anova(limpets_lm)
