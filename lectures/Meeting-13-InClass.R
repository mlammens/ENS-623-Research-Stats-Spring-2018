library(dplyr)

data(iris)
View(iris)

# look at diffs between setosa and versicolor using a t-test
t.test(x = filter(iris, Species == "setosa")$Petal.Length,
       y = filter(iris, Species == "versicolor")$Petal.Length )

# let's look at differences among species
library(ggplot2)

ggplot() +
  geom_boxplot(data = iris, aes(x = Species, y = Petal.Length))

# look at diffs between virginica and versicolor using a t-test
t.test(x = filter(iris, Species == "virginica")$Petal.Length,
       y = filter(iris, Species == "versicolor")$Petal.Length )

# look at diffs between setosa and virginica using a t-test
t.test(x = filter(iris, Species == "setosa")$Petal.Length,
       y = filter(iris, Species == "virginica")$Petal.Length )


# run an anova
anova(lm(data = iris, formula = Petal.Length ~ Species))

summary(lm(data = iris, formula = Petal.Length ~ Species))


# Let's work with a different response variable
ggplot() +
  geom_boxplot(data = iris, aes(x = Species, y = Sepal.Width))

anova(lm(data = iris, Sepal.Width ~ Species))
summary(lm(data = iris, Sepal.Width ~ Species))

# calculate a posthoc test
TukeyHSD(aov(data = iris, Sepal.Width ~ Species))

# Factorial ANOVA
limpets1 <- read.csv(file = "https://mlammens.github.io/ENS-623-Research-Stats/data/Logan_Examples/Chapter12/Data/quinn1.csv")
View(limpets1)

# Convert density into factor
limpets1$DENSITY <- factor(limpets1$DENSITY)
summary(limpets1)

#Visuals
ggplot(data = limpets1) +
  geom_boxplot(aes(x = DENSITY, y = EGGS))

ggplot(data = limpets1) +
  geom_boxplot(aes(x = SEASON, y = EGGS))

ggplot(data = limpets1) +
  geom_boxplot(aes(x = DENSITY, y = EGGS, fill = SEASON))

# run anova model
limpets_lm <- lm(data = limpets1, EGGS ~ SEASON * DENSITY)
anova(limpets_lm)

summary(limpets_lm)


# Do another two-factor model
limpets <- read.csv(file = "https://mlammens.github.io/ENS-623-Research-Stats/data/Logan_Examples/Chapter12/Data/quinn.csv")
summary(limpets)

# Make density a factor
limpets$DENSITY <- factor(limpets$DENSITY)
summary(limpets)

ggplot(data = limpets) +
  geom_boxplot(aes(x = DENSITY, y = EGGS, fill = SEASON))

limpets2_lm <- lm(data = limpets, EGGS ~ SEASON * DENSITY)
anova(limpets2_lm)


# ANCOVA
fruitfly <- read.csv(file = "https://mlammens.github.io/ENS-623-Research-Stats/data/Logan_Examples/Chapter15/Data/partridge.csv")

summary(fruitfly)

# Visualize data
ggplot(data = fruitfly) +
  geom_boxplot(aes(x = TREATMENT, y = LONGEV))

ggplot(data = fruitfly) +
  geom_point(aes(x = THORAX, y = LONGEV))

ggplot(data = fruitfly, aes(x = THORAX, y = LONGEV, colour = TREATMENT)) +
  geom_point() +
  stat_smooth(method = "lm")

# Do ANCOVA
fruitfly_ancova <- lm(data = fruitfly,
                      LONGEV ~ TREATMENT* THORAX)
anova(fruitfly_ancova)

fruitfly_ancova2 <- lm(data = fruitfly,
                       LONGEV ~ TREATMENT + THORAX)
anova(fruitfly_ancova2)

summary(fruitfly_ancova2)
