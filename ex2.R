# IML 2018
# Exercise set 2
setwd("~/R")

##############################################
# Exercise 1
# a
library(MASS)

p <- 2
mu <- c(0,0) # expected values of X_1 and X_2
var1 <- 2 # variance of X_1
var2 <- 3 # variance of X_2
corr12 <- -.75 # desired correlation of X_1 and X_2
cov12 <- sqrt(var1 * var2) * corr12 # solve covariance

n <- 200

sigma <- matrix(c(var1, cov12, cov12, var2), p) # covariance matrix
sigma


set.seed(2345)

X <- mvrnorm(n, mu, sigma) 
cov(X) # covariance matrix of X_1 and X_2 
cor(X)[1,2] # correlation between X_1 and X_2
# Estimates for the covariance matrix and correlation are close to the true parameter values -0.75, but not the same.





# b
par(mfrow = c(2,2))
plot(X, pch = 20, col = 'green', xlab = 'X_1', ylab = 'X_2')

density_est <- kde2d(X[ ,1], X[ ,2])
contour(density_est)
image(density_est)
persp(density_est)

Y <- mvrnorm(n, mu, matrix(c(1,.95,.95,1), p)) # strong linear relationship between the variables
density_est2 <- kde2d(Y[ ,1], Y[ ,2])
plot(Y, pch = 20, col = 'pink', xlab = 'X_1', ylab = 'X_2')
contour(density_est2)
image(density_est2)
persp(density_est2)

Z <- mvrnorm(n, mu, matrix(c(1,0,0,1), p)) # independent variables (2-dimensional standard normal distribution)
density_est3 <- kde2d(Z[ ,1], Z[ ,2])
plot(Z, pch = 20, col = 'blue', xlab = 'X_1', ylab = 'X_2')
contour(density_est3)
image(density_est3)
persp(density_est3)

V <- mvrnorm(1e5, mu, matrix(c(1,0,0,1), p)) # same as Z, but with sample size 10000
density_est4 <- kde2d(V[ ,1], V[ ,2])
plot(V, pch = 20, col = 'grey', xlab = 'X_1', ylab = 'X_2', cex = .2)
contour(density_est4) # much smoother estimated densities
image(density_est4)
persp(density_est4)






# c
# density function of multivariate normal distribution
# x = matrix containing the data, row = data point, col = dimension
# mu = vector of expected values
# sigma = covariance matrix
# return = vector of densities 
density_norm <- function(x, mu, sigma) {
  p <- length(mu)
  ss <- apply(x, 1, function(row) (row - mu) %*% solve(sigma) %*% (row-mu))
  1 / ((2 * pi)^(p/2) * sqrt(det(sigma))) * exp(-ss / 2)
}

# create grid and compute densities for the grid points
grid1 <- .25*(-20:20)
grid2 <- expand.grid(grid1, grid1)
grid_density1 <- density_norm(grid, mu, sigma)
head(grid_density1) # check that density function work correctly
# and it matches the number 1.1307e-19
grid_matrix1 <- matrix(grid_density1, nrow = 41)

contour(grid_matrix1) 
image(grid_matrix1)
persp(grid_matrix1)



# d
mu2 <- c(2,1)

# compute densities for the grid points for the normal distribution with expected value mu = c(2,1)
grid_density2 <- density_norm(grid, mu2, sigma)
grid_matrix2 <- matrix(grid_density2, nrow = 41)
# posterior probabilities for the event, that a data point was generated from the class with mu = (0,0)
#posterior <- grid_matrix1 / (grid_matrix1 + grid_matrix2) 

#contour(posterior)
#persp(posterior)
#image(posterior)

contour(grid_matrix1, col = 'red' )
contour(grid_matrix2, col = 'blue', add = TRUE)
#contour(posterior, add = TRUE)

