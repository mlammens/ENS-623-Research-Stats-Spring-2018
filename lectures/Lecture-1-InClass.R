# Lecture 1 - Notes

# This is R as a calculator
3 + 5 

# Variables and objects

## Numeric objects
(a <- 3)
b <- 5
c <- a + b

# Re-assign a
a <- 10
c <- a + b

## Character objects
d <- "cat"
d

## Vectors - lists of numbers, characters, or factors
my_vector <- c(2, 6, 8, 9, 3)

# Vectors of numbers of equal spacing
(vector_1_10 <- 1:10)
(vector_1_10 <- seq(from = 1, to = 10, by = 1))

(vector_1_100 <- seq(from = 1, to = 100, by = 3))

# Vectors of characters
char_vector <- c("a", "b", "c", "d")

# Example of Matt's mistake
matts_vector <- c(a,b,c,d)
matts_vector

matts_correct_example <- c( d, dog)

## Probability

# Simulate a throw of a coin?
?sample

coin <- c("heads", "tails")
coin_prob <- c(.5, .5)
sample(x = coin, size = 1, replace = TRUE, prob = coin_prob)

# Draw 100 samples
coins_100 <- sample(x = coin, size = 100, replace = TRUE, prob = coin_prob)
table( coins_100)

## Redefine coin_prob
coin_prob <- c(.75, .25)
coins_100_unfair <- sample(x = coin, size = 100, replace = TRUE, prob = coin_prob)
table( coins_100_unfair)

## Matt's M&M counts
blue <- 14
brown <- 9
green <- 9
orange <- 11
red <- 6
yellow <- 6

matts_mms <- c(14,9,9,11,6,6)
matts_tot <- sum(matts_mms)

mm_data <- data.frame(blue = c(13, 17, 10, 12, 13, 20, 14),
                      brown = c(8, 5, 10, 9, 10, 5, 9),
                      green = c(10, 9, 15, 12, 6, 5, 9),
                      orange = c(14, 11, 12, 15, 15, 9, 11),
                      red = c(3, 5, 2, 5, 2, 8, 6),
                      yellow = c(10, 8, 5, 3, 9, 7, 6) )

row.names(mm_data) <- c("Jason", "Marissa", "Kristen", "Bethany", "Alex", "Diane", "Matt")

## Total MMs
sum(mm_data)

## Total of each color
colSums(mm_data)

## How many MMs per bag
bag_tots <- rowSums(mm_data)

## Mean bag total
mean_bag_tots <- mean(bag_tots)

## total deviation
sum(bag_tots - mean_bag_tots)

## total absolute deviation
sum(abs(bag_tots - mean_bag_tots))

## squared deviance
sum((bag_tots - mean_bag_tots)^2)/(length(bag_tots)-1)

var(bag_tots)

sd(bag_tots)

## Prob of green OR blue OR red
sum(c(14,9,6)) / 55
## or
sum( 14/55 + 9/55 + 6/55)

## Colors as a vector
mm_colors <- c("blue", "brown", "green", "orange", "red", "yellow")
mm_probs <- c(.24, .14, .16, .20, .13, .14)

## I want to "sample" a bag of MMs
new_bag <- sample(x = mm_colors, size = 55, replace = TRUE, prob = mm_probs)
table(new_bag)
