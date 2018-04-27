# Multiple regression
c3_plants <- read.csv(file = "https://mlammens.github.io/ENS-623-Research-Stats/data/Logan_Examples/Chapter9/Data/paruelo.csv")

summary(c3_plants)

library(ggplot2)
library(GGally)

ggpairs(c3_plants)

ggplot() +
  #geom_histogram(aes(x = c3_plants$C3, y = ..density..)) +
  geom_density(aes(x = c3_plants$C3))

ggplot(data = c3_plants) +
  geom_point(aes(x = C3, y = MAP))

## Data transformation of C3
c3_plants_orig <- c3_plants

c3_plants$C3 <- log10(c3_plants$C3 + 0.1)

# Also going to transform lat/long
c3_plants$LAT <- as.vector(scale(c3_plants$LAT, scale = FALSE))
c3_plants$LONG <- as.vector(scale(c3_plants$LONG, scale = FALSE))

ggplot() + 
  geom_density(aes(x = scale(c3_plants$LONG, scale = FALSE)))
ggplot() + 
  geom_density(aes(x = scale(c3_plants$LONG, scale = TRUE)))

ggpairs(c3_plants)
summary(c3_plants)

# Actually doing the regression
c3_plants_lm_noint <- lm(data = c3_plants, C3 ~ LONG  + LAT)
plot(c3_plants_lm_noint)

summary(c3_plants_lm_noint)

summary(lm(data = c3_plants, C3 ~ LAT))
summary(lm(data = c3_plants, C3 ~ LONG))

# Do the regresssion with an interactio
c3_plants_lm_int <- lm(data = c3_plants, C3 ~ LONG  * LAT)
summary(c3_plants_lm_int)


# So which model do we use
anova(c3_plants_lm_noint, c3_plants_lm_int)

# AIC
AIC(c3_plants_lm_noint)
AIC(c3_plants_lm_int)
