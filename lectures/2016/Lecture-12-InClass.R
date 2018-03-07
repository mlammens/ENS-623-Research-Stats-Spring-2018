# Lecture 12 - In Class Notes

## GLM - Logistic Regession example

spiders <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter17/Data/polis.csv")

spiders

## Visualize these data
library(ggplot2)

ggplot(data = spiders, aes(x = RATIO, y = PA)) +
  geom_point() +
  theme_bw()

## General linear regression
spider.lm <- lm(data = spiders, formula = PA ~ RATIO)

summary(spider.lm)

ggplot(data = spiders, aes(x = RATIO, y = PA)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()


# Diagnostic plot
plot(spider.lm)

## Fit a generalized linear model (glm)
?glm

spiders.glm <- glm(data = spiders, formula = PA ~ RATIO, family = "binomial")
summary(spiders.glm)

ggplot(data = spiders, aes(x = RATIO, y = PA)) +
  geom_point() +
  theme_bw() +
  stat_smooth(method = "glm", method.args = list(family = "binomial"))

plot(spiders.glm)


## ANCOVA

fruitfly <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter15/Data/partridge.csv")

head(fruitfly)

## Visualize these data

## Longevity based on treatment
ggplot(data = fruitfly, aes(x = TREATMENT, y = LONGEV)) +
  geom_boxplot() +
  theme_bw()


## Longevity based on size
ggplot(data = fruitfly, aes(x = THORAX, y = LONGEV)) +
  geom_point() +
  theme_bw()


## Longevity based on both size and treatment
ggplot(data = fruitfly, aes(x = THORAX, y = LONGEV, colour = TREATMENT)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()


## Build the ANCOVA Model
fruitfly_ancova <- lm(data = fruitfly, LONGEV ~ TREATMENT * THORAX)

summary(fruitfly_ancova)
anova(fruitfly_ancova)

fruitfly_ancova_2 <- lm(data = fruitfly, LONGEV ~ TREATMENT + THORAX)
summary(fruitfly_ancova_2)
anova(fruitfly_ancova_2)


## Frequency Analysis

## Mortality of coolibah trees across riparian dunes
trees <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter16/Data/roberts.csv")

head(trees)

## Make a contingency table
trees_xtab <- table(trees$POSITION, trees$DEAD)
trees_xtab

(trees_chisq <- chisq.test(trees_xtab, correct = FALSE))
 