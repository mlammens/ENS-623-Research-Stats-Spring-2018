# Challenge 1
# Write an exmaple where adding parantheses matters
3 * 5 + 7
3 * (5 + 7)

## Variables and objects
## Two functions that are useful
## str()
## class()

foo <- c(8, 5, 9)
str(foo)
class(foo)

# Practice with variables
my_var <- 8
my_var2 <- 10
my_var + my_var2

my_var_tot = my_var + my_var2 
print(my_var_tot)
my_var_tot
rm(my_var)

my_var <- 3

# What is my_var_tot right now!

## Let's make a vector
my_vect <- c(my_var, my_var2)

## Char vector
pets <- c("cat", "dog", "rabbit", "pig", "snake")
pets

## Make a vector of numbers in sequence
v2 <- 1:10
v2
v3 <- seq(from = 1, to = 10)
v3

## Ask are v2 and v3 the same?
v2 == v3

## Ask are v2 and v3 not the same?
v2 != v3

### Challenge 

# Look up the help page for seq
# 1. Use this to make a vector from 1 to 100, by steps of 5
# 2. Come up with a way to use the `length.out` argument

seq(from = 0, to = 100, by = .1)
seq(from = 0, to = 100, length.out = 17)
