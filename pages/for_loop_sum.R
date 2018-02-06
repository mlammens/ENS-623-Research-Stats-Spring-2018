## ************************************************************************** ##
## for_loop_sum.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2018-02-03
##
## Purpose:
## Sum up a vector of numbers using a for loop. (For practice with for loops.)
##
## ************************************************************************** ##

# Make a vector of numbers 1 through 10
my_vect <- 1:10

# Create a variable to keep track of the cumulative sum of the vector
tot_sum <- 0

# Start for loop
for(x in my_vect){
  # Update the value of tot_sum to add the current number
  tot_sum <- tot_sum + x
}

# Check that the tot_sum value is the same as the result of R's `sum` function
tot_sum == sum(my_vect)
