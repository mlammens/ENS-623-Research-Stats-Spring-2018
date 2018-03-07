# First challenge

## Data - tree heights: 4.2, 5.3, 6.1, 8.9, 5.3, 7.2, 4.7

tree_data <- c(4.2, 5.3, 6.1, 8.9, 5.3, 7.2, 4.7)
mean(tree_data)

## Draw a random number between min and max data values
my_rand <- runif(n = 1, min = min(tree_data), max = max(tree_data))

## Calculate sum of squared differences between my_rand and vector values
sum_sqrd_dev <- sum( (tree_data - my_rand)^2 )

## Let's optimize this
mean_guess <- seq( from = min(tree_data), to = max(tree_data), length.out = 1000)

## Make a function to return sum_sqrd_dev
sum_sqrd_dev_func <- function(my_mean_guess, my_tree_data){
  return( sum( (my_tree_data - my_mean_guess)^2 ) )
}

## Test out my function
sum_sqrd_dev_func( my_mean_guess = my_rand, my_tree_data = tree_data )

## Use this function for every `mean_guess` value
for ( i in mean_guess ){
  print(sum_sqrd_dev_func( my_mean_guess = i, my_tree_data = tree_data ))
}

## Use an sapply function
mean_guess_sqrd_dev <- sapply(mean_guess, sum_sqrd_dev_func, my_tree_data = tree_data)

mean_guess_sqrd_dev

library(ggplot2)
ggplot() +
  geom_point( data = NULL, aes( x = mean_guess, y = mean_guess_sqrd_dev ) ) +
  theme_bw()

## What value is the min sqrd dev?
min(mean_guess_sqrd_dev)
which( mean_guess_sqrd_dev == min(mean_guess_sqrd_dev) )
mean_guess[ 374 ]

mean(tree_data)

## R based estiamte of maximum likelihood: First

## Write a function for nll
neg.ll <- function(x, mu, sigma2){
  sum( 0.5 * log(2 * pi * sigma2) + 0.5 * ((x-mu)^2) / sigma2 )
}

## Let's generate some data

x <- rnorm(n = 1000, mean = 1, sd = 2)
hist(x)

## Generate test values
mu.test.values <- seq(from = -2, to = 4, by = 0.1)
sigma2.test.values <- seq(from = 1, to = 11, by = 0.1)

## Initiate a matrix to save NLL values in
likelihood.matrix <- matrix( nrow = length(mu.test.values),
                             ncol = length(sigma2.test.values) )

## Fill the matrix
for(i in 1:length(mu.test.values)){
  for(j in 1:length(sigma2.test.values)){
    likelihood.matrix[i, j] <- neg.ll(x, mu = mu.test.values[i],
                                      sigma2 = sigma2.test.values[j])
  }
}

image(mu.test.values, sigma2.test.values, likelihood.matrix, col = topo.colors(100))
contour(mu.test.values, sigma2.test.values, likelihood.matrix, nlevels = 30, add = T)

max.element <- which( likelihood.matrix == min(likelihood.matrix), arr.ind = T)
max.element

mu.test.values[ max.element[1] ]
sigma2.test.values[ max.element[2] ]

mean(x)
var(x)

plot( mu.test.values, likelihood.matrix[, max.element[2] ])
plot( sigma2.test.values, likelihood.matrix[max.element[1], ])

## R based estiamte of maximum likelihood: Second
library(MASS)
fitdistr(x, "normal")
?fitdistr

