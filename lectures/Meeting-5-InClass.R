# Meeting 5 - In Class Notes

## Visualizing probability

#install.packages("VennDiagram")

library(VennDiagram)

# Create a venn diagram
draw.single.venn(area = 0.24, 
                 category = "Blue M&M", 
                 fill = "blue", alpha = 0.5)

# Create a venn diagram that shows prob of red and prob of blue
grid.newpage()
draw.pairwise.venn(area1 = 0.24,
                   area2 = 0.13,
                   cross.area = 0,
                   category = c("Blue", "Red"),
                   fill = c("blue", "red"),
                   alpha = c(0.5, 0.5))

# Make venn diagram of prob blue and peanut
grid.newpage()
draw.pairwise.venn(area1 = 0.24,
                   area2 = 0.33,
                   cross.area = 0.076,
                   category = c("Blue", "Peanut"),
                   fill = c("blue", "yellow"),
                   alpha = c(0.5, 0.5))

# Data visualization

data(iris)
head(iris)
tail(iris)

summary(iris)

# differences between factors and characters
iris_new <- iris

iris_new$Species_char <- as.character(iris_new$Species)
summary(iris_new)
table(iris_new$Species)
table(iris_new$Species_char)

table(iris_new$Species[1:100])
table(iris_new$Species_char[1:100])
is.factor(iris_new$Species_char)

iris[1,3]
iris[1:50,1]
iris[51:100,1]

iris[51:100,c(1,5)]

iris[5, ]

head(iris[2:3, ])

iris[2:3]

as.matrix(iris[1:4])[2:3]

head(as.matrix(iris))
head(iris)

iris$Species

head( iris[c("Sepal.Width", "Sepal.Length")])
head( iris[c(1,2)])

iris[c(2,1)]

# Let's add some information
iris_habitat <- 
  data.frame( Species = c("setosa", "versicolor", "virginica"),
              Habitat = c("forest", "wetland", "meadow"))

iris_full <- merge(x = iris, y = iris_habitat, by = "Species")

head(iris_full)

hist(iris_full$Petal.Length)

hist(iris_full$Petal.Length, breaks = 30)

# Visualize relationships between multiple variables

plot(x=iris_full$Sepal.Length, y = iris_full$Sepal.Width)

library(ggplot2)

# Simple xy scatter plot
ggplot(data = iris_full, 
       aes(x=Sepal.Length, y = Petal.Length,
           colour = Species, shape = Species)) +
  geom_point() +
  geom_smooth(method = "lm")

# Box plots

ggplot(data = iris_full, 
       aes(x = Species,
           y=Sepal.Length,
           colour = Species)) +
  geom_boxplot()
  

ggplot(data = iris_full, 
       aes(x=Sepal.Length,
           fill = Species)) +
  geom_histogram(position = "dodge")

ggplot(data = iris_full, 
       aes(x=Sepal.Length,
           fill = Species)) +
  geom_density(alpha = 0.5)
