# Meeting 7

# Draw 10000 samples from a standard normal dist
stdnorm_samps <- rnorm(n = 10000, mean = 0, sd = 1)

library(ggplot2)

ggplot(data = NULL, aes(x = stdnorm_samps)) +
  geom_histogram(aes( y = ..density.. ))

sd(stdnorm_samps)
mean(stdnorm_samps)

# add a density plot
ggplot(data = NULL, aes(x = stdnorm_samps)) +
  geom_histogram(aes( y = ..density.. )) +
  geom_density(colour = "red", size = 2)

# add the functional form of the normal
ggplot(data = NULL, aes(x = stdnorm_samps)) +
  geom_histogram(aes( y = ..density.. )) +
  geom_density(colour = "red", size = 2) +
  stat_function( fun = dnorm, colour = "blue", 
                 alpha = 0.6, size = 2)

# Get our quantiles
qnorm(p = 0.025, mean = 0, sd = 1)
qnorm(p = 0.975, mean = 0, sd = 1)

ggplot(data = NULL, aes(x = stdnorm_samps)) +
  geom_histogram(aes( y = ..density.. )) +
  geom_density(colour = "red", size = 2) +
  stat_function( fun = dnorm, colour = "blue", 
                 alpha = 0.6, size = 2) +
  geom_vline(xintercept = c(-1.96, 1.96), size = 2)



# Sample from poisson
my_pois_sample <- rpois(100, lambda = 1)
ggplot() +
  geom_histogram(aes(x = my_pois_sample, y = ..density..))

mean(my_pois_sample)
sd(my_pois_sample)

# standard error of the mean
sd(my_pois_sample) / sqrt(100)
