# Let's do a t-test!
gastro <- read.csv(file = "https://mlammens.github.io/ENS-623-Research-Stats/data/Logan_Examples/Chapter6/Data/ward.csv")

summary(gastro)

# Let's make a plot!

#install.packages("ggplot2") # For Norman

library(ggplot2)

ggplot() +
  geom_boxplot(data = gastro,
               aes(x = ZONE, y = EGGS)) +
  theme_bw()

# Run a t-test
gastro_t_test <- t.test(data = gastro, EGGS ~ ZONE,
                        var.equal = TRUE)
gastro_t_test

t.test(x = gastro[1:42,2], y = gastro[43:79,2],
       var.equal = TRUE)

# By hand
littor <- gastro[43:79,2]
mussel <- gastro[1:42,2]

s_y1y2 <- 
  sqrt( (((length(littor)-1)*var(littor)) + 
           ((length(mussel)-1)*var(mussel)))/
         (length(littor) + length(mussel) - 2) * 
          ((1/length(littor)) + (1/length(mussel)))
        )
(mean(littor)-mean(mussel)) / s_y1y2

pt(q = 5.3899, df = 77, lower.tail = FALSE) * 2


# Let's do another example!!!!
fulmars <- read.csv(file = "https://mlammens.github.io/ENS-623-Research-Stats/data/Logan_Examples/Chapter6/Data/furness.csv")

summary(fulmars)
ggplot() +
  geom_boxplot(data = fulmars,
               aes(x = SEX, y = METRATE)) +
  theme_bw()

library(dplyr)
fulmars %>% 
  group_by(SEX) %>%
  summarise(Mean = mean(METRATE), Var = var(METRATE))

t.test(data = fulmars, METRATE~SEX, var.equal = FALSE)
t.test(data = fulmars, METRATE~SEX, var.equal = TRUE)
