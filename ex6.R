# IML 2018
# Exercise set 6
setwd("~/R")

##############################################
# Exercise 1
# a
# load mnist
source('loadmnist.R')
load_mnist()

# function to compute pairwise distances efficiently using only R built-in functions.
# this is taken from here: https://www.r-bloggers.com/pairwise-distances-in-r/
vectorized_pdist <- function(A,B) { 
  an = apply(A, 1, function(rvec) crossprod(rvec,rvec))
  bn = apply(B, 1, function(rvec) crossprod(rvec,rvec))
  
  m = nrow(A)
  n = nrow(B)
  
  tmp = matrix(rep(an, n), nrow=m) 
  tmp = tmp +  matrix(rep(bn, m), nrow=m, byrow=TRUE)
  sqrt( tmp - 2 * tcrossprod(A,B) )
}
train_x <- train$x[1:5000,]
train_y <- train$y[1:5000]

test_x <- test$x[1:1000,]
test_y <- test$y[1:1000]

dd <- vectorized_pdist(train_x,test_x)
dd[1,1] # distance between the first training and the first test point


# Exercise 2
# a
set.seed(1234)

# define parameters
n <- 100
mu <- 0
sigma <- sqrt(100)
m <- 1000

# how many times the true mean is inside the interval
inside_interval <- 0

# radius of the confidence interval
r <- 1.96*(sigma/sqrt(n)) 

# sample 'm' data sets of size 'n'
X <- matrix(rnorm(n*m,mean = mu,sd = sigma), nrow = m)

for(ii in 1:m) {
  
  est_mu <- mean(X[ii,])
  
  if(est_mu - r < mu && mu < est_mu + r) {
    inside_interval <- inside_interval + 1
  }
}

cat("The proportion of confidence intervals that include the true parameter: ", inside_interval/m)
