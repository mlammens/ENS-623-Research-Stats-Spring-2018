# Make an example
x <- 1:10
y <- 10:1 + rnorm(10, sd = 5)

library(ggplot2)
ggplot() +
  geom_point(aes(x = x, y = y))

cor(x,y)

cor(x, 1:10)

cor.test(x = x, y = y)
?cor.test

# Linear regression
## Change in y is due to your change in x

flr_beetle <- read.csv(file = "https://mlammens.github.io/ENS-623-Research-Stats/data/Logan_Examples/Chapter8/Data/nelson.csv")
flr_beetle

# Let's plot these data
ggplot(data = flr_beetle, aes(x = HUMIDITY, y = WEIGHTLOSS)) +
  geom_point() +
  geom_smooth(method = "lm")

# run a linear regression
flr_beetle_lm <- lm(data = flr_beetle, 
                    WEIGHTLOSS ~ HUMIDITY)
summary(flr_beetle_lm)

ggplot() +
  geom_histogram(aes(x = flr_beetle_lm$residuals))

flr_beetle_lm$residuals

# Access diagnostic plots
plot(flr_beetle_lm)


## Multiple linear regression
bird_frag <- read.csv(file = "https://mlammens.github.io/ENS-623-Research-Stats/data/Logan_Examples/Chapter9/Data/loyn.csv")
summary(bird_frag)

#install.packages("GGally")
library(GGally)
ggpairs(bird_frag)

bird_frag_transform <- bird_frag

bird_frag_transform$AREA <- log10(bird_frag_transform$AREA)
bird_frag_transform$DIST <- log10(bird_frag_transform$DIST)
bird_frag_transform$LDIST <- log10(bird_frag_transform$LDIST)

ggpairs(bird_frag_transform)

# make a linear model
bird_frag_lm <- lm(data = bird_frag, 
                   ABUND ~ log10(AREA) + YR.ISOL + log10(DIST) +
                     log10(LDIST) + GRAZE + ALT)

summary(bird_frag_lm)

summary( lm(data = bird_frag, 
            ABUND ~ YR.ISOL + log10(DIST) +
              log10(LDIST) + GRAZE + ALT) )

plot(bird_frag_lm)

# ***

