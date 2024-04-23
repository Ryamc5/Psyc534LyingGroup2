## Final Project

#Read in data set for student study performance

study_performance <- read.csv("~/Downloads/study_performance.csv")

#calculate median reading score
median_score <- median(study_performance$reading_score)

par(mfrow = c(2, 2))

#create a histogram of reading score

hist(study_performance$reading_score,
     breaks = seq(0, 100, by = 10),
     main = "Breakdown of Reading Scores",
     xlab = "Reading Score",
     ylab = "Frequency",
     col = "skyblue",
     border = "black",
     xlim = c(0, 100),
     ylim = c(0, max(table(cut(study_performance$reading_score, breaks = seq(0, 100, by = 10)))) + 5),
     labels = TRUE)
#add red line to show median
abline(v = median_score, col = "red", lwd = 2)

#create a new object for our faked data
study_performance$lying <- rep(NA, length(study_performance$reading_score))

fill_bottom_25 <- function(data) {
  quantile_25 <- quantile(data, 0.25)
  data[data <= quantile_25] <- TRUE
  data

  
}
#fill in the top 75% from reading score into lying
study_performance$lying <- fill_bottom_25(study_performance$reading_score)

#create a function to replace the bottom 25% with values from the top 25%
#as of right now, everything that was in the bottom percentile is represented by a 1
# in the lying function. due to this, we can write code that replaces all the 1s
replace_1_with_top_quartile <- function(data, top_values) {
  set.seed(123)
  top_quartile <- quantile(top_values, 0.75)  # Find the 75th percentile of top_values
  indices_to_replace <- which(data == 1)  # Identify indices where the value is 1
  top_quartile_values <- top_values[top_values >= top_quartile]  # Select values from the top quartile of top_values
  data[indices_to_replace] <- sample(top_quartile_values, length(indices_to_replace), replace = TRUE)  # Replace 1s with random values from the top quartile of top_values
  data
}


study_performance$lying_replaced_with_top_25th_percentile <- replace_1_with_top_quartile(study_performance$lying, study_performance$reading_score)

hist(study_performance$lying_replaced_with_top_25th_percentile,
     breaks = seq(0, 100, by = 10),
     main = "Breakdown of Reading Scores With Lying (top 25 replaces bottom 25)",
     xlab = "Reading Score",
     ylab = "Frequency",
     col = "skyblue",
     border = "black",
     xlim = c(0, 100),
     ylim = c(0, max(table(cut(study_performance$lying_replaced_with_top_25th_percentile, breaks = seq(0, 100, by = 10)))) + 5),
     labels = TRUE)
#add red line to show original median
abline(v = median_score, col = "red", lwd = 2)
#add purple line to show median of new data
abline(v = median(study_performance$lying_replaced_with_top_25th_percentile), col = "purple", lwd =2)

# to go one step further, lets see what happens if we allow the bottom 25% of scores to be
# replaced by any value in the top 50% of scores
replace_1_with_top_half <- function(data, top_values) {
  set.seed(123)
  top_half <- quantile(top_values, 0.5)  # Find the 50th percentile of top_values
  indices_to_replace <- which(data == 1)  # Identify indices where the value is 1
  top_half_values <- top_values[top_values >= top_half]  # Select values from the top half of top_values
  data[indices_to_replace] <- sample(top_half_values, length(indices_to_replace), replace = TRUE)  # Replace 1s with random values from the top quartile of top_values
  data
}

study_performance$lying_replaced_with_top_50th_percentile <- replace_1_with_top_half(study_performance$lying, study_performance$reading_score)

hist(study_performance$lying_replaced_with_top_50th_percentile,
     breaks = seq(0, 100, by = 10),
     main = "Breakdown of Reading Scores With Lying (top 50 replaces bottom 25)",
     xlab = "Reading Score",
     ylab = "Frequency",
     col = "skyblue",
     border = "black",
     xlim = c(0, 100),
     ylim = c(0, max(table(cut(study_performance$lying_replaced_with_top_50th_percentile, breaks = seq(0, 100, by = 10)))) + 5),
     labels = TRUE)
#add red line to show original median
abline(v = median_score, col = "red", lwd = 2)
#add purple line to show median of new data
abline(v = median(study_performance$lying_replaced_with_top_50th_percentile), col = "purple", lwd =2)

# finally, lets see what happens if we allow the bottom 25% to be replaced
# by any value in the top 75% of scores

replace_1_with_top_75 <- function(data, top_values) {
  set.seed(123)
  top_75 <- quantile(top_values, 0.25)  # Find the 25th percentile of top_values
  indices_to_replace <- which(data == 1)  # Identify indices where the value is 1
  top_75_values <- top_values[top_values >= top_75]  # Select values from the top 75% of top_values
  data[indices_to_replace] <- sample(top_75_values, length(indices_to_replace), replace = TRUE)  # Replace 1s with random values from the top quartile of top_values
  data
}

study_performance$lying_replaced_with_top_75th_percentile <- replace_1_with_top_75(study_performance$lying, study_performance$reading_score)

hist(study_performance$lying_replaced_with_top_75th_percentile,
     breaks = seq(0, 100, by = 10),
     main = "Breakdown of Reading Scores With Lying (top 75 replaces bottom 25)",
     xlab = "Reading Score",
     ylab = "Frequency",
     col = "skyblue",
     border = "black",
     xlim = c(0, 100),
     ylim = c(0, max(table(cut(study_performance$lying_replaced_with_top_75th_percentile, breaks = seq(0, 100, by = 10)))) + 5),
     labels = TRUE)
#add red line to show original median
abline(v = median_score, col = "red", lwd = 2)
#add purple line to show median of new data
abline(v = median(study_performance$lying_replaced_with_top_75th_percentile), col = "purple", lwd =2)

