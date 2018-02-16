tot_mm_pop_sample <- c(15, 13, 17)

# Calculate mean mm in bag
sum(tot_mm_pop_sample)/length(tot_mm_pop_sample)

mean(tot_mm_pop_sample)

# Calculate a range
range(tot_mm_pop_sample)

min(tot_mm_pop_sample)

max(tot_mm_pop_sample)

# Max minus min
max(tot_mm_pop_sample) - min(tot_mm_pop_sample)


# sum of deviation from mean
tot_mm_pop_sample <- c(15, 13, 17, 13, 15, 14, 14, 13, 16, 15, 13, 16, 15, 15, 16)

mm_mean <- mean(tot_mm_pop_sample)

ind_deviations <- tot_mm_pop_sample - mm_mean
ind_deviations

sum(ind_deviations)

# absolute deviation
sum(abs(ind_deviations))

# standard deviation and variance
square_ind_deviations <- ind_deviations^2
square_ind_deviations
(sum_square_ind_deviations <- sum(square_ind_deviations))

# going to variance
sum_square_ind_deviations/ (length(square_ind_deviations)- 1)
var(tot_mm_pop_sample)

# *********************#

# clear all of our data
rm(list = ls())

# read in our data
mm_data <- read.csv(file.choose())


mm_colors <- c("blue", "brown", "green", "orange", "red", "yellow")
mm_probs <- c(0.23, 0.14, 0.16, 0.20, 0.13, 0.14)
(sum(mm_probs))

new_bag <- sample(x = mm_colors, size = 15, replace = TRUE, prob = mm_probs)
new_bag_tally <- table(new_bag)

new_bag_tally

my_orig_bag <- mm_data$Bag.1
my_orig_bag
my_orig_bag <- my_orig_bag[1:6]

# does my bag match the random bag
new_bag_tally == my_orig_bag

new_bag_counts <- c(sum(new_bag == "blue"),
                    sum(new_bag == "brown"),
                    sum(new_bag == "green"),
                    sum(new_bag == "orange"),
                    sum(new_bag == "red"),
                    sum(new_bag == "yellow"))
new_bag_counts
new_bag_tally

all(my_orig_bag == new_bag_counts)
