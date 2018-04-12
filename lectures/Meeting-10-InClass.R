# Non-normally distributed data
lizard <- read.csv(file = "https://mlammens.github.io/ENS-623-Research-Stats/data/Logan_Examples/Chapter6/Data/beetle.csv")
head(lizard)

summary(lizard)

library(ggplot2)

ggplot() + 
  geom_boxplot(data = lizard, aes(x = SIZE, y = BEETLES)) +
  theme_bw()

ggplot() +
  geom_histogram(data = lizard, aes(x = BEETLES, fill = SIZE)) +
  facet_grid( .~SIZE) +
  theme_bw()

ggplot() +
  geom_histogram(data = lizard, aes(x = BEETLES, fill = SIZE)) +
  facet_grid( SIZE~.) +
  theme_bw()

# Mann-Whitney test

# step 1 - rank data
lizard_ordered <- lizard[order(lizard$BEETLES), ]

# step 2 = add a rank value column to data.frame
lizard_ordered$rank <- 1:45

# Calculate rank sums of the different groups
library(dplyr)

lizard_ordered %>%
  group_by(SIZE) %>%
  summarise(Rank_Sum = sum(rank))


## Challenge

# 1. Calculate the t-stat for differences in the number of
#    beetles consumed by the two size classes of lizards.
# 2. Randomize / shuffle the data.
# 3. Calculate the **new** t-stat.
# 4. Repeat 999 times
# 5. Compare the observed t-stat the the generated t-stats

# Run t-test
t.test( data = lizard, BEETLES~SIZE)

# Get t-stat by it self
t_obs <- t.test( data = lizard, BEETLES~SIZE)$statistic

# Randomize my data
lizard_shuffled <- data.frame(SIZE = sample(lizard$SIZE),
                              BEETLES = lizard$BEETLES)

(t_rand <- t.test( data = lizard_shuffled, BEETLES~SIZE)$statistic)




##

t_orig <- t.test(data = lizard, BEETLES ~ SIZE)$statistic

## Initiate vector of new t-stat values
t_rand <- c()

## Set the number of reps (shuffles) we want to use
reps <- 1000

## Begin for loop to randomize data
for ( int in 1:reps){
  
  ## Shuffle the data
  lizard_shuffled <- data.frame(SIZE = sample(lizard$SIZE), BEETLES = lizard$BEETLES)
  
  ## Run the t-test on the new data, and save the t-stat to the t_rand vector
  t_rand <- c( t_rand,
               t.test(data = lizard_shuffled, BEETLES ~ SIZE)$statistic )
  
}

ggplot() + 
  geom_histogram(data = NULL, aes(x = t_rand)) +
  geom_vline(xintercept = t_orig, colour = "red") +
  theme_bw()

t_rand_extreme <- sum(abs(t_rand) > t_orig)

(p_t_orig <- t_rand_extreme/ reps)

# Multiple testing
my_data <- list()
for( i in 1:20){
  my_data[[i]] <- rnorm(10)
}

p_vals <- matrix(ncol = 20, nrow = 20)
p_vals

for(i in 1:19){
  for(j in (i+1):20){
    p_vals[i, j] <- t.test(my_data[[i]], my_data[[j]])$p.value
  }
}
p_vals

sum(p_vals<0.05, na.rm = TRUE)

###

# Covariation
data(iris)
head(iris)

ggplot(data = iris) +
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  theme_bw()

var(iris$Sepal.Length)
var(iris$Sepal.Width)

# Calculate covariance
var(iris$Sepal.Length, iris$Sepal.Width)
cor(iris$Sepal.Length, iris$Sepal.Width)

iris %>%
  group_by(Species) %>%
  summarise(Cor = cor(Sepal.Length, Sepal.Width))
