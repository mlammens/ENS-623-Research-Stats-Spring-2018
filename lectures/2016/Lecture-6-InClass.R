# Differences in fecundity of intertidal gastropods in two different intertidal zones

## Load data and peek at it
gastropod <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter6/Data/ward.csv")

View(gastropod)
summary(gastropod)

## Plot the data
library(ggplot2)

ggplot() +
  geom_boxplot(data = gastropod, aes(x = ZONE, y = EGGS)) +
  theme_bw()

## Calcualte "group means and variances"

# if need to install, do this
# install.packages("dplyr")
library(dplyr)

gastropod %>%
  group_by(ZONE) %>%
  summarise(Mean = mean(EGGS), Var = var(EGGS))
  
## Run a t-test
gastropod_t_test <- t.test(data = gastropod, EGGS ~ ZONE, var.equal = TRUE )

t.test(data = gastropod, EGGS ~ ZONE, var.equal = FALSE )

pt(-5.3899, df = 77)

gastropod_t_test$estimate
gastropod_t_test$estimate[1] - gastropod_t_test$estimate[2]
gastropod_t_test$conf.int

## northern fulmar - metabolic rate 
fulmars <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter6/Data/furness.csv")
fulmars

summary(fulmars)

# Let's plot these data
ggplot() +
  geom_boxplot(data = fulmars, aes(x = SEX, y = METRATE)) +
  theme_bw()

## Cacluate the means and variance
fulmars %>%
  group_by(SEX) %>%
  summarise(Mean = mean(METRATE), Var = var(METRATE))

# Run t-test using unequal variances
(fulmars_t_test <- t.test(data = fulmars, METRATE ~ SEX, var.equal = FALSE))

## Non-parametric tests
?wilcox.test

# Randomization test
# Differences in beetle consumption of eastern horned lizards
lizard <- read.csv(file = "https://mlammens.github.io/Biostats/data/Logan_Examples/Chapter6/Data/beetle.csv")

summary(lizard)

## Make a box plot
ggplot() + 
  geom_boxplot(data = lizard, aes(x = SIZE, y = BEETLES)) +
  theme_bw()

## Make histogram plot
ggplot() + 
  geom_histogram(data = lizard, aes(x = BEETLES, fill = SIZE)) +
  facet_grid( . ~ SIZE) +
  theme_bw()

## Randomization approach
# 1. calculate t stat for the observed data
# 2. randomize our data / shuffle
# 3. calculate the new t-stat using this suffled data, repeat 999 times
# 4. compare the original t-stat to the distribution of the random values

## 1. calculate t-stat
t_orig <- t.test(data = lizard, BEETLES ~ SIZE)$statistic
t_orig

## Initiate a vector to save new t-stat values
t_rand <- c()

## Set number of reps
reps <- 1000

for ( int in 1:reps){
  ## Make a shuffled data.frame
  lizard_shuffled <- data.frame(SIZE = sample(lizard$SIZE),
                                BEETLES = lizard$BEETLES )
  
  ## Run a t.test on shuffled data, and save to my t_rand vector
  t_rand <- c( t_rand,
               t.test(data = lizard_shuffled, BEETLES ~ SIZE)$statistic )
}

ggplot() +
  geom_histogram(data = NULL, aes(x = t_rand)) +
  geom_vline(xintercept = t_orig, colour = "red") +
  theme_bw()

t_rand_extreme <- sum(abs(t_rand) >= t_orig)
t_rand_extreme
t_rand_extreme / reps

## Multiple comparisons problem

## Get 20 different samples from standard normal
my_data <- list()
for (i in 1:20) {
  my_data[[i]] <- rnorm(10)  #Note the double brackets for a list
}

## Make a matrix to save p-vals in
p_vals <- matrix(ncol = 20, nrow = 20)

## Carry out all possible pair-wise comparisons
for (i in 1:19) {
  for (j in (i + 1):20) {
    p_vals[i, j] <- t.test(my_data[[i]], my_data[[j]])$p.value
  }
}

p_vals

20*20/2 - 20

180 * .05
sum(p_vals < 0.05, na.rm = TRUE)
