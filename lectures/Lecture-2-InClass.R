# Lecture 2 - In Class Notes

## if/else statements and for loops

### if/else for coin flip

coin_flip_number <- runif(n = 1)

if( coin_flip_number > 0.5){
  coin_flip <- "heads"
} else {
  coin_flip <- "tails"
}

coin_flip

### ifelse for coin flip
?ifelse
coin_flip <- ifelse( test = runif(1) > 0.5,
                     yes = "heads",
                     no = "tails" )

## What if I want to simulate this 100 times

### Let's us a for loop

## Repicates
rep <- 100
## Make an empty vector for saving the results of our coin flips
all_flips <- c()

for( ind in 1:rep){
  print("I'm in a coinflip loop!")
  print(ind)
  
  coin_flip <- ifelse(runif(1) > 0.5, yes = "heads", no = "tails")
  print(coin_flip)
  
  ## Add this flip to all_flips
  all_flips[ind] <- coin_flip

}

## Summarize our data
table(all_flips)

## Get the number of heads
sum(all_flips == "heads")

sum(all_flips == "tails")

# Our major challange!!! 

## Colors as a vector
mm_colors <- c("blue", "brown", "green", "orange", "red", "yellow")

## Proportion/probability of each color
mm_probs <- c(.24, .14, .16, .20, .13, .14)

## I want to "sample" a bag of MMs
new_bag <- sample(x = mm_colors, size = 55, replace = TRUE, prob = mm_probs)

# Website
# mlammens.github.io/Biostats

mm_dataset <- read.csv("data/class_mm_data.csv", row.names = 1)

my_bag <- mm_dataset["Matt", ]
my_bag


## Use a for loop to generate new bags of M&Ms, and find out how many match your observed bag.

mm_colors <- c("blue", "brown", "green", "orange", "red", "yellow")
mm_probs <- c(.24, .14, .16, .20, .13, .14)

count <- 0

for( ind in 1:300000){
  #Sample a new bag
  new_bag <- sample(x = mm_colors, size = 55, replace = TRUE, prob = mm_probs)

  ## Count the nuber of each colour
  new_bag_counts <- c( sum(new_bag == "blue"),
                       sum(new_bag == "brown"),
                       sum(new_bag == "green"),
                       sum(new_bag == "orange"),
                       sum(new_bag == "red"),
                       sum(new_bag == "yellow") )
  
  ## Check to see if my bag is the same as the new bag
  if( all(my_bag == new_bag_counts) ){
    count <- count +1
  }
}


## Data exploration

rm( list=ls())


## Let's work with the iris dataset
data(iris)
View(iris)

## Let's have a peek at these data
head(iris)
tail(iris)

## Look a summary of these data
summary(iris)

## Distinction between factors and characters
summary(iris$Species)
## versus
summary(as.character(iris$Species))
?as.factor

## Navigating a data.frame
iris
# 5th row, 3rd colum
iris[5, 3]

# all of the 3rd column
iris[ , 3]

# Call the column by the column name
iris$Petal.Length

iris$Petal.Length[5]

## Get a whole row
iris[5, ]

## Get sets of rows or columns
iris[5:10, ]

iris[5:10, 1:3]

head( iris[2:3] )

## Just virginica

### Hack
iris_virginica <- iris[ 101:150, ]
head(iris_virginica)

iris_virginica2 <- iris[which(iris$Species == "virginica"), ]


## Let's make some plots
plot( x = 1:150, y = iris$Petal.Length)

hist( iris$Petal.Length)

hist(iris$Petal.Length, breaks = 30 )

## Relationshiops between two variables
plot( x = iris$Sepal.Length, y = iris$Petal.Length)

## Try plotting with `ggplot2`
##install.packages("ggplot2")

## Load a package
library(ggplot2)

## Let's fire up ggplot
ggplot() +
  geom_point( data = iris, aes( x = Sepal.Length,
                                y = Petal.Length,
                                colour = Species,
                                shape = Species) ) +
  geom_smooth( data = iris, aes( x = Sepal.Length,
                                y = Petal.Length,
                                colour = Species), 
               method = "lm" ) +
  geom_smooth( data = iris, aes( x = Sepal.Length,
                                 y = Petal.Length), 
               method = "lm", linetype = "dashed" ) +
  theme_bw()

## Hisgtogram
ggplot( data = iris, aes( x = Petal.Length, fill = Species) ) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(alpha = .2) +
  facet_grid( Species ~ . ) +
  theme_bw()
