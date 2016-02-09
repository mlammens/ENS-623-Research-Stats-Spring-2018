## Start with solution to HW 1

## Simulate the battle between the foxes, given red fox attacks first
bout <- runif(1)

if( bout < 0.6 ){
  print("red fox wins")
} else {
  print("arctic fox wins")
}

## Embed the battle in a for loop and run 1000 times

## Initiate the redfox_total_wins counter
redfox_total_wins <- 0

for( ind in 1:1000){
  ## Simulate a full match between the foxes
  
  ## Initiate win counts
  redfox_wins <- 0
  arcticfox_wins <- 0
  
  ## Initiate redfox attacks first
  redfox_attacks <- TRUE
  
  while( all(redfox_wins < 21, arcticfox_wins <21) ){
    
    if(redfox_attacks){
      bout <- runif(1)   
      if( bout < 0.6 ){
        print("red fox wins")
        redfox_wins <- redfox_wins + 1
        redfox_attacks <- TRUE
      } else {
        print("arctic fox wins")
        arcticfox_wins <- arcticfox_wins + 1
        redfox_attacks <- FALSE
      }  
    } else if(!redfox_attacks) {
      bout <- runif(1)   
      if( bout < 0.5 ){
        print("red fox wins")
        redfox_wins <- redfox_wins + 1
        redfox_attacks <- TRUE
      } else {
        print("arctic fox wins")
        arcticfox_wins <- arcticfox_wins + 1
        redfox_attacks <- FALSE
      }
    }
  }
  if(redfox_wins >= 21){
    print("red fox wins overall")
    redfox_total_wins <- redfox_total_wins + 1
  }
} # End for loop



## New stuff

## What is the probability that red fox wins x bouts out of n total bouts

## Initiate red fox wins vector
redfox_bouts_wins_vector <- c()

for( foo in 1:1000){
  ## Probability of red fox winning is 0.6
  redfox_wins <- 0
  
  for( bar in 1:100){
    bout <- runif(1)   
    if( bout < 0.6 ){
      print("red fox wins")
      redfox_wins <- redfox_wins + 1
    } else {
      print("arctic fox wins")
    }  
  }
  redfox_bouts_wins_vector <- c(redfox_bouts_wins_vector, redfox_wins)
}

## Make a histogram - base R
hist( x = redfox_bouts_wins_vector )

## Make a histogram - ggplot
library(ggplot2)

ggplot() +
  geom_histogram( data = NULL,
                  aes(x = redfox_bouts_wins_vector,
                      y = ..density..),
                  binwidth = 1) + 
  xlab( "Number of bouts red fox wins") +
  ylab( "Probability") +
  theme_bw()

# Prob 60 wins followed by 40 losses
.6^60 * .4^40

(factorial(100)/(factorial(60)*factorial(40))) * .6^60 * .4^40

(factorial(100)/(factorial(60)*factorial(40)))


## Use distributions in R
runif(1)
?runif
?rnorm
?rbinom


dbinom(x = 60, size = 100, prob = 0.6)

## Playing with the normal
normal_rvs <- rnorm( n = 100000, mean = 0, sd = 1)

ggplot() +
  geom_histogram(data = NULL, aes(x = normal_rvs)) +
  theme_bw()

mean(normal_rvs)
sd(normal_rvs)
