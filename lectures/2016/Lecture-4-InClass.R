# In class notes - lecture 4

1*1/8 + 2*1/8 + 3*1/8 + 4*1/8 + 5*1/8 + 6*3/8

(1/6) * sum(1:6)

## Draw samples from standard normal

stdnorm_samps <- data.frame( samps = rnorm( n = 10000, mean = 0, sd = 1) )

stdnorm_samps$samps

library(ggplot2)

ggplot(data = stdnorm_samps, aes(x = samps)) +
  geom_histogram( aes(y = ..density..) ) +
  geom_density(colour = "red", size = 2) +
  stat_function( fun = dnorm, colour = "blue", alpha = 0.6, size = 2) +
  theme_bw()


## ---------------------------------------------------------———————— ##

my_data <- rnorm( 1000, mean = 50, sd = 10)

mean(my_data)
sd(my_data)


my_data_ztransformed <- (my_data - mean(my_data)) / sd(my_data)

mean(my_data_ztransformed)
sd(my_data_ztransformed)

## ---------------------------------------------------------———————— ##
## 95% interval

?qnorm
qnorm( p = 0.025)
qnorm( p = 0.975)

qnorm( p = 0.025, mean = 10)
qnorm( p = 0.975, mean = 10)

ggplot(data = stdnorm_samps, aes(x = samps)) +
  coord_cartesian(xlim = c(-5, 5), ylim = c(0, 0.41)) +
  geom_vline(xintercept = c(-1.96, 1.96), size = 2 ) +
  stat_function( fun = dnorm, colour = "blue", size = 2) +
  theme_bw()

## ---------------------------------------------------------———————— ##
## The Central Limit Theorem

### The distribution of sample means is

#### Challenge

# 1. Take 100 samples of 10 observations from a normal distribution with a mean of 0 and a standard deviation of 1. Calculate the mean of each sample. Plot a histogram of the means.
# 2. Increase the number of samples. How does this affect the outcome?
# 3. Increase the number of observations. How does this affect the outcome?

# Part 1. 
my_means <- c()

num_samples <- 10000
num_obs <- 100

for ( ind in 1:num_samples){
  # Draw observations from standard normal
  obs_from_dist <- rnorm(10, mean = 10)
  
  # Calculate mean of obs
  mean_of_obs <- mean(obs_from_dist)
  
  # Add the mean value to my_means
  my_means <- c( my_means, mean_of_obs)
}

ggplot() +
  geom_histogram(data = NULL, aes( x = my_means, y = ..density..) ) +
  theme_bw()

mean(my_means)
sd(my_means)


## Use a different distribution

ggplot() +
  geom_histogram( data = NULL, aes( x = rpois(1000, lambda = 2), y = ..density..) ) +
  theme_bw()


my_means <- c()

num_samples <- 10000
num_obs <- 100

for ( ind in 1:num_samples){
  # Draw observations from standard normal
  obs_from_dist <- rpois(10, lambda = 2)
  
  # Calculate mean of obs
  mean_of_obs <- mean(obs_from_dist)
  
  # Add the mean value to my_means
  my_means <- c( my_means, mean_of_obs)
}

ggplot() +
  geom_histogram(data = NULL, aes( x = my_means, y = ..density..) ) +
  theme_bw()

## ---------------------------------------------------------———————— ##

## Standard error of the mean


