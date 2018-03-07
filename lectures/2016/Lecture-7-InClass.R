## Covariation

## Use the iris dataset

data(iris)
head(iris)

library(ggplot2)

ggplot() +
  geom_point(data = iris, aes(x = Sepal.Length, y = Sepal.Width) ) +
  theme_bw()

## Calculate the covariation
covar <- sum( (iris$Sepal.Length - mean(iris$Sepal.Length)) *
                (iris$Sepal.Width - mean(iris$Sepal.Width)) ) / (nrow(iris) - 1)
covar

var(x = iris$Sepal.Length, y = iris$Sepal.Width)
cov(x = iris$Sepal.Length, y = iris$Sepal.Width)

cov(x = (iris$Sepal.Length*10), y = iris$Sepal.Width)


?cov

## Correlation
covar

cor_iris <- covar / (sd(iris$Sepal.Length)*sd(iris$Sepal.Width))

cor_iris

se_cor_iris <- sqrt( (1-cor_iris^2) / (nrow(iris)-2) )
se_cor_iris

(t_cor_iris <- cor_iris / se_cor_iris)

pt(q = t_cor_iris, df = (nrow(iris)-2) ) * 2

## Use cor.test to find the correlation
cor.test(x = iris$Sepal.Length, y = iris$Sepal.Width, method = "pearson")


## Linear regression
flr_beetle <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter8/Data/nelson.csv")
flr_beetle

ggplot() +
  geom_point(data = flr_beetle, aes(x = HUMIDITY, y = WEIGHTLOSS)) +
  theme_bw()

## Fit a linear model
flr_beetle_lm <- lm(data = flr_beetle, formula = WEIGHTLOSS ~ HUMIDITY)

summary( flr_beetle_lm )

ggplot(data = flr_beetle, aes(x = HUMIDITY, y = WEIGHTLOSS)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()
