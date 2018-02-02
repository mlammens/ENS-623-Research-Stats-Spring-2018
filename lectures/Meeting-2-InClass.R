# Meeting 2 - In Class

# Exploring variable elements

pets <- c("cat", "dog", "rabbit", "pig", "snake")
pets[1]

# get elements in sequence
pets[3:4]

pets[c(1,2)]
pets[1:2]
pets[seq(1,2,by=1)]

# non sequential
pets[c(5,1)]

# repeat numbers
pets[c(5,1,1)]

# make an index vector, and use that to call elements
ind_vector <- c(5,1,1)
pets[ind_vector]

# Matrices

# Make a matrix
set.seed(8)
my_mat <- matrix(data = runif(50), nrow = 10, byrow = TRUE)
my_mat

matrix(data = 1:9, nrow = 3, byrow = TRUE)
matrix(data = 1:9, nrow = 3, byrow = FALSE)

matrix(data = 1:9, nrow = 4, byrow = TRUE)

# Navigate elements of the matrix
my_mat[1,1]
my_mat[2,1]
my_mat[1:4,1:3]

my_mat[1, ]
my_mat[ , c(1,3,5)]

row.names(my_mat) <- 1:10
colnames(my_mat) <- 1:5
head(my_mat)

row.names(my_mat)
row.names(my_mat) <- letters[1:10]
row.names(my_mat)

# Combine internal functions with matrices

mean(my_mat)
sd(my_mat)

# get row and column means
rowMeans(my_mat)
colMeans(my_mat)

?apply
apply(X = my_mat, MARGIN = 1, FUN = sd)
apply(X = my_mat, MARGIN = 2, FUN = sd)

apply(X = my_mat, MARGIN = 1, FUN = mean)
rowMeans(my_mat)

range(my_mat)
apply(X = my_mat, MARGIN = 1, FUN = range)

# User defined functions

fahr_to_kelvin <- function(temp) {
  kelvin <- ((temp - 32) * (5/9)) + 273.15
  return(kelvin)
}

fahr_to_kelvin
fahr_to_kelvin()
fahr_to_kelvin(32)
fahr_to_kelvin(temp = 32)

seq(1,10, by = 1)
seq(from = 1,to = 10, by = 1)
?seq
seq(to = 1,from = 10)

kelvin_to_celsius <- function(temp) {
  celsius <- temp - 273.15
  return(celsius)
}

kelvin_to_celsius(temp = 273.15)
temp
celsius

# Challenge - write a function to convert from FAHR to CELSIUS

fahr_to_celsius <-function(temp){
  celsius<-(temp-32)*(5/9)
  return(celsius)
}

fahr_to_celsius <- function(temp){
  new_temp <- fahr_to_kelvin(temp)
  celsius <- kelvin_to_celsius(new_temp)
  return(celsius)
}

fahr_to_celsius(-5)

my_mat[1,1] <- 99

# for loops

# for (variable in a collection) {
#   do things with that vairable
# }

# Make a temperature data set
set.seed(8)
temp_data <- runif(n = 20, min = -5, max = 5) + 45
temp_data

for( x in temp_data){
  fahr_to_kelvin(x)
}

for( x in temp_data){
  print("x =")
  print(x)
  print(fahr_to_kelvin(x))
}

for( x in 1:length(temp_data)){
  print("x =")
  print(x)
  print(fahr_to_kelvin(temp_data[x]))
}


# use for loop and store new values
temp_data_kelvin <- vector()
for( x in 1:length(temp_data)){
  print("x =")
  print(x)
  print(fahr_to_kelvin(temp_data[x]))
  temp_data_kelvin[x] <- fahr_to_kelvin(temp_data[x])
}
temp_data_kelvin

# Conditionals

# if / else

TRUE
FALSE

20 == 20

20 > 40

# !
!(20>40)

## Challenge

x <- TRUE
y <- FALSE

x & y

x | y

xy <- c(x, y)

xy

any(xy)
all(xy)

foo <- runif(10)
bar <- foo >= 0.5
bar
as.numeric(bar)
sum(bar)

# if statement

num <- 37
if (num > 100){
  print("greater")
} else {
  print("not greater")
}

if (num > 100){
  print("greater")
}

# cascade of if/else
num = 100

greater_less_equal <- function(num){
  if(num > 0) {
    return("greater")
  } else if (num == 0){
    return("it's zero!")
  } else if (num < 0){
    return("less than")
  }
}
# Make the above into a function, that takes
# any numeric value, and returns whether it is
# positive, negative, or equal to 0