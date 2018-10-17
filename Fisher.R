rm(list = ls())
gc(reset = TRUE)
graphics.off()
library(shotGroups)

set.seed(123) # seed is necessary to make it reproducible

# let us generate data
count <- 500
labelsNr <- 3
labelsJ <- 1 : 1 : labelsNr
labelsI <- 1 : 1 : count

sigma1 <- matrix(c(4, 2, 2, 3), ncol = 2)
sigma2 <- matrix(c(2, - 2, - 2, 6), ncol = 2)
sigma3 <- matrix(c(4, - 4, - 4, 5), ncol = 2)
mean1 = c(2, 0)
mean2 = c(1, 10)
mean3 = c(6, 7)

library(mvtnorm)

Sigmas = list(sigma1, sigma2, sigma3)
Means = list(mean1, mean2, mean3)
x <- matrix(, count * labelsNr, 3)

for (j in seq(along = labelsJ)) {
  sigma = Sigmas[[j]]
  mean = Means[[j]]
  xx <- rmvnorm(n = count, mean = mean, sigma = sigma)
  a <- switch(j, "red", "green", "blue");
  par(new = TRUE)
  for (i in seq(along = labelsI)) {
    x[(j - 1) * count + i, 1 : 2] = xx[i,]
    x[(j - 1) * count + i, 3] = j
  }
}

#split the data in proportion 70/30 for training and validation purposes.
sample_size <- floor(0.7 * nrow(x))

train_ind <- sample(seq_len(nrow(x)), size = sample_size)

train_set <- x[train_ind,]
test <- x[- train_ind,]
train <- train_set

fisher_score <- function(data, best_count, class_count){
  points_count <- nrow(data)
  label_col <- ncol(data)
  calc_dim_count <- label_col - 1
    for (j in 1:calc_dim_count) {
      dim_mean <- mean(data[, j])
      dim_result <- 0 
      for (i in 1:class_count) {
        lables<-data[, label_col]
        mask<- lables == i
        current_class_dim_Points <- data[mask, j]
        class_point_count <- length(current_class_dim_Points)
        class_mean <- mean(current_class_dim_Points)
        fraction_class_points <- class_point_count/points_count
        point_to_mean <- 0
        for (elem in current_class_dim_Points) {
          value <- (elem - class_mean)^2
          point_to_mean <- point_to_mean + value
        }
        dispersion <- point_to_mean/(class_point_count - 1)
        class_mean_to_full_mean <- (class_mean - dim_mean)^2
        upper <- fraction_class_points * class_mean_to_full_mean
        under <- fraction_class_points * dispersion
        dim_result = dim_result + (upper/under)
      }
  }
}

fisher_score(test, 2, 3)