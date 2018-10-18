
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

covariance <- function(data){
  dimension_count <- ncol(data)
  cov_matrix <- matrix( ,nrow = dimension_count, ncol = dimension_count)
  
  for (i in 1:dimension_count) {
    for (j in 1:dimension_count) {
      
      cov <- 0
      if (i == j){
        dim_mean <- mean(data[, i])
        col_vector <- data[, i]
        for (elem in col_vector) {
          value <- (elem - dim_mean)^2
          cov <- cov + value
        }
      }
      else
      {
        i_dim_mean <- mean(data[, i])
        j_dim_mean <- mean(data[, j])
        i_col_vector <- data[, i]
        j_col_vector <- data[, j]
        
        i_value <- 0
        for (r in 1:nrow(data)) {
            i_value <- i_col_vector[r] - i_dim_mean
            j_value <- j_col_vector[r] - j_dim_mean
            value <- i_value * j_value
            
            cov <- cov + value
        }
      }
      
      final <- cov / (nrow(data) - 1)
      
      cov_matrix[i, j] = final
    }
  }
  return (cov_matrix)
  
}

malanhobisfun<- function(point, mean, data){
  subtracted_vector = point - mean
  #transpose
  transposed_vector = t(subtracted_vector)
  
  #covariance_matrix
  cov_of_dataset = cov(data)
  
  # find the inversion,  S^-1
  inv_cov = solve(cov_of_dataset)
  
  # difference times S^-1 times the difference transposed
  full_result = subtracted_vector %*% inv_cov %*% transposed_vector
  
  final = full_result ^ (1/2)
  print(final)
  return(final)
  
}

#dataset taken from here https://jamesmccaffrey.wordpress.com/2017/11/09/example-of-calculating-the-mahalanobis-distance/
Dataset = matrix(
  c(
    64.0,  66.0,  68.0,  69.0,  73.0,
    580.0, 570.0, 590.0, 660.0, 600.0,
    29.0,  33.0,  37.0,  46.0,  55.0
  ),
  nrow=5,
  ncol=3
)


p = matrix(c(64, 580, 29), nrow=1)

m = colMeans(Dataset)

a = c(4,4,3,4,5)
b = c(1,2,3,7,9)

g = malanhobisfun(p, m, Dataset)

value = minkowsky(a,b,1)
value2 = minkowsky(a,b,2)
value3 = minkowsky(a,b,3)
value4 = minkowsky(a,b,4)
value5 = minkowsky(a,b,5)
value6 = minkowsky(a,b,Inf)
canb = canberradist(a,b)
