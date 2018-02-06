## ************************************************************************** ##
## while_loop_example.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2018-02-04
##
## Purpose:
## Gives an example of a while loop
##
## ************************************************************************** ##

# Let's use a while loop to count the number of times you need to toss a fair 
# die to get a 6.

# Simulate the die with a list of values 1 to 6
dice <- 1:6

# Use a while loop to "throw" the die, until it lands on a six

# Set a tracker for whether you got a six or not. It should start at FALSE
got_six <- FALSE

# Set a tracker to count the number of dice tosses
toss_count <- 0

while(got_six != TRUE){
  # Add 1 to the toss_count to indicate we made a toss
  toss_count <- toss_count + 1
  
  # Toss the die - I'm using the sample function for this
  die_val <- sample(x = dice, size = 1)
  
  # Check to see if the value was size, if it was, then change got_six
  if(die_val == 6){
    got_six <- TRUE
  }
}

# The number of tosses it took is what ever toss_count is now
# This value could change every time you run the code above,
# since it's a random process.
print(toss_count)