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


D_cov = cov(train[,1:2])
D_cov_inv = solve(D_cov)


mahalanobis <- function(a, b){
  v = a - b
  result = v %*% D_cov_inv %*% v
  return(result ^ 0.5)
}

canberradist<- function(element1, element2){
  summ <- 0
  for (i in seq(along=element1)) {
    
    sub_abs <- abs(element1[i] - element2[i])
    abs_sum <- abs(element1[i]) + abs(element2[i])
    summ <- summ + sub_abs/abs_sum
  }
  return(summ)
}


minkowsky<- function(element1,element2, p){
  
  dimensions=length(element1)
  sqd<-matrix(,dimensions,1)
  
  if(p ==Inf){
    for(i in seq(along=element1)){
      sqd[i]<-abs(element1[i]-element2[i])
    }
    dist<-max(sqd)
  }
  
  else{
    summ <- 0
    for(i in seq(along=element1)){
      summ<- summ + abs(element1[i]-element2[i])^p
    }
    dist <-summ^(1/p)
  }
  return(dist)
}


#for arguments takes data matrix with labels, new point to add and how many nearest neighbours
knnfun <- function(data, new_point, neighbours_count){
  
  #copy data matrix to the new matrix to add distance column
  copied_data_matrix <- matrix(data, ncol=ncol(data))
  #should use NA instead of 0? 
  data_matrix_extra_column <- cbind(copied_data_matrix, rep(0,nrow(copied_data_matrix)))
  distance_column_index <- ncol(data_matrix_extra_column)
  #calculate distance to every point and add it to the distance column
  for (data_point_index in 1:nrow(data)) {
    current_point <- data[data_point_index, 1:2]
    distance <- minkowsky(new_point, current_point, Inf)
    data_matrix_extra_column[data_point_index, distance_column_index] = distance
  }
  #sort matrix increasing 
  sorted_neighbours_matrix <- data_matrix_extra_column[order(data_matrix_extra_column[, distance_column_index]),]
  
  #take matrix neighbours count matrix rows
  k_nearest_neighbours <-sorted_neighbours_matrix[1:neighbours_count,]
  
  labels_as_vector <- as.vector(k_nearest_neighbours[, 3])
  #let neighbours vote for the new point lable
  
  best_elem = -1
  best = -1
  for (i in labels_as_vector){
    count = sum(labels_as_vector == i)
    if (count > best){
      best = count
      best_elem = i
    }
  }
  return(best_elem)
}


#Takes initial data, new points and neighbour count
lable_new_points <- function(data, test_points, neighbour_count){
  lable_column <- ncol(test_points)
  initial_points = matrix(test_points, ncol=ncol(test_points))
  
  for (i in 1:nrow(test_points)) {
    current_point <- test_points[i, 1:2]
    current_point_lable <- knnfun(data, current_point, neighbour_count)
    test_points[i,lable_column] = current_point_lable
  }
  
  for (i in 1:nrow(data)) {
    c <- data[i, lable_column]
    a <- switch(c, "red", "blue", "purple", "yellow", "green", "orange", "cyan", "coral", "chartreuse4", "aquamarine4", "aquamarine2")
    par(new = TRUE)
  }
  count <- 0
  #prints new points, if old label does not equal new lable then print as reqtangle
  for (i in 1:nrow(test_points)) {
    c <- test_points[i, lable_column]
    c_actual = initial_points[i, lable_column]
    pch = 1
    if (c_actual != c){
      count = count + 1
      pch = 17
    }
    a <- switch(c, "red", "blue", "purple", "yellow", "green", "orange", "cyan", "coral", "chartreuse4", "aquamarine4", "aquamarine2")
    plot(test_points[i, 1], test_points[i, 2], col = a, type = "p", pch=pch, xlim = c(- 10, 20), ylim = c(- 10, 20))
    par(new = TRUE)
  }
  print(count)
  
}

b <-lable_new_points(train, test, 5)
