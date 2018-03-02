## Start a `for` loop, to replicate a known number of bouts

## Set the probability that a red fox wins the bout
prob_redfox_win <- 0.6

## Set the number of bouts
bouts <- 100 

## Initiate the number of redfox bout wins
redfox_bout_win <- 0


for(ind in 1:bouts){
  
  
  ## Draw a random number to simulate the outcome of the bout
  bout_outcome <- runif(1)
  
  ## Use an if statement to determine who won
  if(bout_outcome < prob_redfox_win){ # red fox wins
    ## Print the outcome to the console, so we know who won
    print("red fox wins")
    
    ## Increase the number of redfox_bout_win by 1
    redfox_bout_win <- redfox_bout_win + 1
    
  } else { # arctic fox wins
    ## Print the outcome to the console
    print("arctic fox wins")
    
    ## NOTE: We're only interested in the number of redfox wins, so 
    ## we're not going to bother keeping track of the arctic fox wins.
  }  
} # End the `for` loop

print(paste("Red fox won", redfox_bout_win, "bouts!"))


## ************************************************************************** ##

# Set the probability that a red fox wins the bout
prob_redfox_win <- 0.6

## Set the number of replicates
reps <- 100

## Initiate a vector to store the number of red fox wins for each replicate
redfox_bout_wins_vector <- c()

## Start a for loop to go through the desired number of replicates
for(rep in 1:reps){ 
  
  ## Set the number of bouts
  bouts <- 100 
  
  ## Initiate the number of redfox bout wins
  redfox_bout_win <- 0
  
  ## Start for loop to go through the number of bouts
  for(ind in 1:bouts){
    
    
    ## Draw a random number to simulate the outcome of the bout
    bout_outcome <- runif(1)
    
    ## Use an if statement to determine who won
    if(bout_outcome < prob_redfox_win){ # red fox wins
      ## Print the outcome to the console, so we know who won
      #print("red fox wins")
      
      ## Increase the number of redfox_bout_win by 1
      redfox_bout_win <- redfox_bout_win + 1
      
    } else { # arctic fox wins
      ## Print the outcome to the console
      #print("arctic fox wins")
      
      ## NOTE: We're only interested in the number of redfox wins, so 
      ## we're not going to bother keeping track of the arctic fox wins.
    }  
  } # End the `for` loop
  
  print(paste("Red fox won", redfox_bout_win, "bouts!"))
  
  ## Add the number of redfox_bout_win to redfox_bout_wins_vector
  redfox_bout_wins_vector <- c(redfox_bout_wins_vector, redfox_bout_win)
  
}

sum(redfox_bout_wins_vector == 60) / 100

hist(redfox_bout_wins_vector)

library(ggplot2)

ggplot() +
  geom_histogram(data = NULL, 
                 aes(x = redfox_bout_wins_vector), 
                 binwidth = 1) +
  theme_bw()

ggplot() +
  geom_histogram(data = NULL, 
                 aes(x = redfox_bout_wins_vector,
                     y = ..density.. ), 
                 binwidth = 1) +
  theme_bw()


## ************************************************************************** ##

ggplot() +
  geom_histogram(data = iris, 
                 aes(x = Petal.Length, y = ..density.., fill = Species)) +
  facet_grid( Species ~ . ) +
  geom_density(data = iris, aes(x = Petal.Length, colour = Species)) +
  theme_bw()  

## ************************************************************************** ##

?rnorm

rnorm(100)

normal_rvs <- rnorm(n = 100, mean = 0, sd = 1)

ggplot() + 
  geom_histogram( data = NULL, aes(x = normal_rvs) ) +
  theme_bw()