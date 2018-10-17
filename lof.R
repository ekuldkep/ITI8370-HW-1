rm(list = ls())
gc(reset = TRUE)
graphics.off()
# clustering and different distance functions. 
# clear everything
library(shotGroups)

# let us generate data
amount = 70
labelsNr <- 1
labelsJ <- 1 : 1 : labelsNr
labelsI <- 1 : 1 : amount

sigma1 <- matrix(c(4, 2, 2, 3), ncol = 2)
sigma2 <- matrix(c(2, - 2, - 2, 6), ncol = 2)
sigma3 <- matrix(c(4, - 4, - 4, 5), ncol = 2)
mean1 = c(2, 0)
mean2 = c(1, 10)
mean3 = c(10, 7)
#Sigmas=data.frame(sigma1,sigma2,sigma3)
Sigmas = list(sigma1, sigma2, sigma3)
Means = list(mean1, mean2, mean3)
x <- matrix(, labelsNr * amount, 3)
library(mvtnorm)



for (j in seq(along = labelsJ)) {
  sigma = Sigmas[[j]]
  mean = Means[[j]]
  xx <- rmvnorm(n = amount, mean = mean, sigma = sigma)
  a <- switch(j, "red", "green", "blue");
  
  par(new = TRUE)
  for (i in seq(along = labelsI)) {
    x[(j - 1) * amount + i, 1 : 2] = xx[i,]
    x[(j - 1) * amount + i, 3] = j
  }
}
# NB!!!!!!!!   variable x will appear in the workspace. Ignore third column

#split the data in proportion 70/30 for training and validation purposes.
sample_size <- floor(0.7 * nrow(x))

# set.seed(123) # seed is necessary to make it reproducible
train_ind <- sample(seq_len(nrow(x)), size = sample_size)

train_set <- x[train_ind,]
test <- x[- train_ind,]

train <- train_set[, 1 : 2]

mydistfun<- function(element1,element2, p){
  # this function returns the distace between the element1 and element2
  dimensions=length(element1)
  sqd<-matrix(,dimensions,1)
  for(i in seq(along=element1)){
    sqd[i]<-abs(element1[i]-element2[i])^p
  }
  dist <-colSums(sqd)^(1/p)
  
  return(dist)
}

lof <- function(data, data_point, k){
  data_point_lk <-lk(data, data_point, k)
  #print(data_point)
  
  ar_data_point <- ar(data, data_point_lk, data_point, k)
  
  lof <- 0
  
  for (i in 1:k) {
    neighbour <-data_point_lk[i, 1:2]
    neighbour_lk <- lk(data, neighbour, k)
    ar_neighbour <- ar(data, neighbour_lk, neighbour, k)
    lof_neighbour <- ar_data_point/ar_neighbour
    lof <- lof + lof_neighbour
  }
  lof <- lof/k
  return(lof)
}


ar <- function(data, data_point_lk, data_point, k){
  ar <-0
  
  for (i in 1:k) {
    neighbour <-data_point_lk[i, 1:2]
    neighbour_lk <- lk(data, neighbour, k)
  
    neighbour_vk <- vk(neighbour_lk, k)
    dist_data_point_to_neighbour <- mydistfun(neighbour,data_point ,2)
    
    rk <- max(neighbour_vk, dist_data_point_to_neighbour)
    ar <- ar + rk
  }
  ar <- ar/k
  return(ar)
}


lk <- function(data, data_point, k){
  new_data_matrix <- matrix(data, ncol=ncol(data))
  data_matrix_distance_column <- cbind(new_data_matrix, rep(NA, nrow(new_data_matrix)))
  distance_column <- ncol(data_matrix_distance_column)
  
  for (i in 1:nrow(data)) {
    data_point_i <- data[i, ]
    if(data_point_i != data_point){
      distance <- mydistfun(data_point, data_point_i, 2)
      data_matrix_distance_column[i, distance_column] = distance
    }
  }
  sorted_neighbours_matrix <- data_matrix_distance_column[order(data_matrix_distance_column[, distance_column]),]
  k_nearest_neighbours <- sorted_neighbours_matrix[1:k,]
  return (k_nearest_neighbours)
}


vk <- function(k_nearest_neighbours, k){

    nearest_far <- k_nearest_neighbours[k,]
    Vk <- nearest_far[3] # the third column is the distance

    return(Vk)
}

#a <- c(1, 2)
#a_lof = lof(train, a, 3)
#print(a_lof)

for (i in 1:nrow(train)) {
   color = "green"
   a_lof = lof(train, train[i,], 7)
   print(a_lof)
   if (a_lof > 1.1){
     color = "yellow"
   }
   if (a_lof > 1.5){
     color = "red"
   }
   plot(train[i, 1], train[i, 2], col = color, type = "p", xlim = c(- 10, 10), ylim = c(- 10, 10))
   par(new = TRUE)
}






