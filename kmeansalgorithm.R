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
mean1 = c(-5, 0)
mean2 = c(-2, 15)
mean3 = c(14, 4)

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

train <- train_set[, 1 : 2] # the data we used was initially prepared for the classificatrion example please remove third column

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

generate_distance_matrix = function(points){
  
  points_size = nrow(points)
  distance_matrix = matrix(,nrow=points_size, ncol=points_size)
  
  for (i in 1:points_size){
    A = points[i, ]
    for (j in i:points_size){
      B = points[j, ]
      distance = minkowsky(A, B, 2)
      distance_matrix[i, j] = distance
      distance_matrix[j, i] = distance_matrix[i, j]
    }
  }
  
  return(distance_matrix)
}

generate_silhouette = function(labels, dmatrix){
  
  points_size = length(labels)
  
  result = matrix(labels, nrow = points_size)
  result = cbind(result, rep(0, points_size))
  
  for (i in 1:points_size){
    label = labels[i]
    dist_row = dmatrix[i, ]
    label_mask = labels == label
    not_label_mask = labels != label
    
    local_cluster_distances = dist_row[label_mask]
    local_cluster_mean_distance = mean(local_cluster_distances)
    
    not_local = dist_row[not_label_mask]
    not_local_labels = labels[not_label_mask]
    
    
    not_local_min_distance = min(not_local)
    
    score = (
      not_local_min_distance - local_cluster_mean_distance
    ) / max(not_local_min_distance, local_cluster_mean_distance)
    
    result[i, 2] = score
  }
  
  
  return(result)
}

sintra = function(cluster_label, labels, dmatrix){
  mask = labels == cluster_label
  local_cluster_matrix = dmatrix[mask, mask]
  summed = sum(local_cluster_matrix)
  len = length(local_cluster_matrix)
  average = summed / len
  return(average)
}

sinter = function(cluster_label, labels, dmatrix){
  local_mask = labels == cluster_label
  not_local_mask = labels != cluster_label
  local_cluster_matrix = dmatrix[local_mask, not_local_mask]
  
  summed = sum(local_cluster_matrix)
  len = length(local_cluster_matrix)
  average = summed / len
  return(average)
}

kmeans <- function(data, cluster_count, iterations=15){
  
  #dimensions count
  columnCount <- ncol(data)
  
  #define new empty matrix where columns count is dimensions count and row count is cluster count
  centroids <- matrix(, ncol=columnCount, nrow=cluster_count)
  
  #generates centroids coordinates by assigning x,y... to be each from random point
  for (clusterIndex in 1:cluster_count) {
    for (columIndex in 1:columnCount) {
      centroid_current_dimension_random_value <- sample(train[, columIndex], size = 1)
      centroids[clusterIndex, columIndex] = centroid_current_dimension_random_value
    }
  }
  
  #https://stackoverflow.com/a/19591226, http://r.789695.n4.nabble.com/help-to-add-a-new-column-filled-with-value-1-td3034361.html
  copied_data_matrix <- matrix(data, ncol=ncol(data))
  #add new column for cluster labels
  data_matrix_extra_column <- cbind(copied_data_matrix, rep(0,nrow(copied_data_matrix)))
  contains_cluster_column <-ncol(data_matrix_extra_column)
  
  #calculate distance and best cluster for each point
  for (simulate in 1:iterations){
    for (i in 1:nrow(data)){
      current_point <- data[i, ]
      best_distance <- Inf
      best_centroid <- NULL
      for (c in sample(1:nrow(centroids))){
        current_centroid <- centroids[c, ]
        distance <- minkowsky(current_point, current_centroid, 2)
        if(distance < best_distance){
          best_distance <- distance
          best_centroid <- current_centroid
          data_matrix_extra_column[i, contains_cluster_column] =  c
        }
      }
    }
    
    # calculate new centroids
    old_centroids = matrix(centroids, ncol=ncol(centroids))
    for (c in 1:nrow(centroids)){
      # a list of indexes where value == c
      mask <- data_matrix_extra_column[,contains_cluster_column] == c
      # select all rows which index is in mask,
      # select all columns, except the last one, which is the c label
      currentClusterPoints <- data_matrix_extra_column[mask,1:columnCount,drop=FALSE]
      # calculate the new mean for this cluster points
      new_mean = colMeans(currentClusterPoints)
      # as some points might not have any points at all, assign them some random one
      claimed_points_count = nrow(currentClusterPoints)
      if (claimed_points_count != 0) {
        centroids[c, ] = new_mean
      }
      if (claimed_points_count == 0 & simulate != iterations)
      {
        # assign random point
        centroids[c, ] = data[c * simulate, ]
      }
    }
    
    difference = old_centroids - centroids
    
    if (sum(abs(difference))<0.2){
      break
    }
  }
  
  if(columnCount < 3){
     for (i in 1:nrow(data_matrix_extra_column)) {
         c <- data_matrix_extra_column[i, contains_cluster_column]
         a <- switch(c, "red", "blue", "purple", "yellow", "green", "orange", "cyan", "coral", "chartreuse4", "aquamarine4", "aquamarine2")
         plot(data_matrix_extra_column[i, 1], data_matrix_extra_column[i, 2], col = a, type = "p", xlim = c(- 10, 20), ylim = c(- 10, 20))
         par(new = TRUE)
     }
    
     for (i in 1:cluster_count) {
         plot(centroids[i, 1], centroids[i, 2], col = "black", pch=17, type = "p", xlim = c(- 10, 20), ylim = c(- 10, 20))
         par(new = TRUE)
     }
  }
  return(data_matrix_extra_column)
}

data_matrix_cluster_column <- kmeans(train, 3)
columns = ncol(data_matrix_cluster_column)
distance_matrix <- generate_distance_matrix(data_matrix_cluster_column[, 1:columns-1])
#shilouette
shilouette <- generate_silhouette(data_matrix_cluster_column[, columns], distance_matrix)
print("silhouette")
print(mean(shilouette[,2]))

#intra/inter
score = 0
clusters <- 3
for (label in 1:clusters){
  intra = sintra(label, data_matrix_cluster_column[, columns], distance_matrix)
  inter = sinter(label, data_matrix_cluster_column[, columns], distance_matrix)
  score = score + intra / inter
}

print("intra/inter")
print(score / clusters)









