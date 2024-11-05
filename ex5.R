# IML 2018
# Exercise set 5
setwd("~/R")

##############################################
# Exercise 3
# a
loydk_means <- function(X, K, mu = NULL, max_iterations = 200) {
  n <- ncol(X)
  p <- nrow(X)
  C <- NULL
  C_old <- 0
  if(is.null(mu)) {
    mu <- matrix(numeric(K * p), nrow = p) # cluster centerpoints
    C <- sample(K, n, replace = TRUE)      # allocate points into K clusters randomly
  } 
  
  for(j in 1:max_iterations) {
    # compute new cluster centerpoints
    if(!is.null(C)) {
      for(i in 1:K) mu[ ,i] <- rowMeans(X[ ,C == i, drop = FALSE])
      C_old <- C
    }
    
    # compute distances to the cluster centerpoints 
    distances <- matrix(numeric(K * n), nrow = K)
    for(i in 1:K) distances[i, ] <- colSums((X - mu[ ,i])^2)
    
    # update the cluster membership labels for each of the data points.
    C <- max.col(-t(distances)) 
    
    # test for convergence 
    if(identical(C, C_old)) { 
      cat("Converged in", j, "iterations.\n")
      return(list(mu = mu, C = C))
    }
  }
  cat("Failed to converge in", max_iterations, "iterations\n")
  list(mu = mu, C = C)
}

# generate the data set
n <- 100
p <- 2
X <- matrix(rnorm(p * n), nrow = 2)


# test that our implemenation works correctly
par(mfrow = c(2,2), mar = c(2,2,4,4))
for(i in 1:4) {
  km <- loydk_means(X, 3)
  plot(X[1, ], X[2, ], pch = 20, cex = .5, asp = 1, col = km$C+1)
  lines(km$mu[1, ], km$mu[2, ], type = 'p', lwd = 4, col = (1:3)+1)
}
# b
source('loadmnist.R')

load_mnist()
n_train <- 500
X <- t(train$x[1:n_train, ])

show_digit(X[ ,5])
train$y[5]
