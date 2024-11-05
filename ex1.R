# IML 2018
# Exercise set 1
 setwd("~/R")

##############################################
# Exercise 1
# b
#function returns the interval 
hoeffding_interval <- function(p,n,alpha = 0.05) {
  
  epss <- sqrt(log(2/alpha)/(2*n) )
  
  lower <- n*(p - epss)
  upper <- n*(p + epss)
  
  return(c(lower,upper))
}
#function to test the Hoeffding interval
test_hoeffding <- function(p,n, ntests = 10000, alpha = 0.05) {
  
  n_successes_all <- rbinom(ntests, n, p)
  inside_interval <- 0
  
  interval <-hoeffding_interval(p,n,alpha = alpha)
  
  lower <- interval[1]
  upper <- interval[2]
  
  for(k in n_successes_all) {
    
    if(lower <= k & upper  >= k) {
      inside_interval = inside_interval + 1
    }
  }
  
  return(inside_interval/ntests)  
}
#Check how many times the outcome is inside the interval
pars <- expand.grid(n = c(10,100,1000),p = c(0.5,0.9,0.99) )

for(ii in 1:nrow(pars)) {
  p <- pars[ii,]$p
  n <- pars[ii,]$n
  
  prop <- test_hoeffding(p,n,alpha = 0.05)
  
  cat("p = ",p, ", n = ",n,", prop = ",prop,"\n",sep="")
  
}
#All  case the proportion is greater than 0.95, which is in line with the conservativity of the Hoeffding bounds.

#d
hoeffding_interval_k <- function(p,n,k,alpha = 0.05) {
  
  epss <- sqrt(log(2*k/alpha)/(2*n) )
  
  lower <- n*(p - epss)
  upper <- n*(p + epss)
  
  return(c(lower,upper))
}
test_hoeffding_k <- function(p,n,k, ntests = 10000, alpha = 0.05) {
  
  
  all_inside = ntests
  
  interval <- hoeffding_interval_k(p,n,k,alpha = alpha)
  lower <- interval[1]
  upper <- interval[2]
  
  for(t in 1:ntests) {
    
    n_successes_k <- rbinom(k, n, p) # draw k values from Bin(n,p)
    
    if(any(n_successes_k < lower) | any(n_successes_k > upper) ) {
      all_inside <- all_inside - 1
    }
    
  }
  
  return(all_inside/ntests)  
}
#
pars <- expand.grid(n = c(10,100,1000),p = c(0.5,0.9,0.99), k = c(10,100) )

for(ii in 1:nrow(pars)) {
  p <- pars[ii,]$p
  n <- pars[ii,]$n
  k <- pars[ii,]$k
  
  prop <- test_hoeffding_k(p,n,k,alpha = 0.05)
  
  cat("p = ",p, ", n = ",n,", k = ",k,", prop = ",prop,"\n",sep="")
  
}
# Exercise 2
# a
path2data <- "datasets/"
college <- read.csv(paste(path2data,"College.csv",sep = ""))

# b
rownames(college) <- college[ ,1]
fix(college)

college <- college[ ,-1]


str(college)

# c
summary(college)
pairs(college[ ,1:10])
plot(college$Private, college$Outstate, col = 3:4, varwidth = T)
str(college)

# iv
Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)

summary(college)
plot(college$Elite, college$Outstate, col = 5:6, varwidth = T)

# v
par(mfrow = c(2,2)) # divide window into 2*2
hist(college$Outstate, col = 'grey')
hist(college$Outstate, col = 'pink', breaks = 50) 
hist(college$Outstate, col = 'green', breaks = 5)
hist(college$Outstate, col = 'beige', breaks = 100)
par(mfrow = c(1,1)) # remember to change graphic parameters back to normal
hist(college$Outstate, col = 'pink', breaks = 10) 
# as you can see typing ?hist, the number of breaks is a suggestion only...


##############################################
# Exercise 3
set.seed(1627) # set seed for the random number generator to make the results duplicable.
n_train <- 30
x <- runif(n_train, -3 ,3)
y <- 2 + x - .5 * x^2 + rnorm(n_train, 0, 0.4)
train <- data.frame(x,y)


# (a)
# generate grid for plotting
grid <- data.frame(x = seq(-3,3, by = .01))

K <- 10
MSE <- numeric(K+1)
models <- list(K)

# constant model (polynomial of order 0)
model0<- lm(y ~ 1, data = train) 
plot(train$x, train$y, main = "K = 0")
lines(grid$x, predict(model0, newdata = grid), col = 'red')
MSE[1] <- mean(residuals(model0)^2)  

# fit polynomials of order 1-10
for(i in 1:K) {
  models[[i]] <- lm(y ~ poly(x,i), data = train)
  plot(train$x,train$y, main = paste("K =", i))
  lines(grid$x, predict(models[[i]], newdata = grid), col = 'red')
  MSE[i+1] <- mean(residuals(models[[i]])^2)  
}

# print and plot the training set mean squared error
MSE
plot(MSE, type = 'b', col = 'red', pch = 18, xlab = 'K')

##################################
# (b)
# generate the test set
n_test <- 1000
x <- runif(n_test, -3 ,3)
y <- 2 + x - .5 * x^2 + rnorm(n_test, 0, 0.4)
test <- data.frame(x,y)

# compute the MSE for the test set
test_MSE <- numeric(K+1)
test_MSE[1] <- mean((mean(train$y) - test$y)^2) # constant model (K = 0)
for(i in 1:K) 
  test_MSE[i+1] <- mean((predict(models[[i]], newdata = test) - test$y)^2)  

# plot & print MSE for the test and training sets 
plot(0:K, MSE, type = 'b', col = 'red', pch = 18, xlab = 'K')
lines(0:K, test_MSE, col = 'green', type = 'b', pch=18)
legend("topright", legend = c('training', 'test'), col = c('red', 'green'), lwd = 1)
test_MSE

# MSE decreases sharply until in both the test and training set until K = 2. Then 
# MSE on the training set continues to decrease slowly. The MSE on the test set depends on 
# the randomly generated data sets. With most of the random seeds the test set error starts
# to slowly increase after K = 2, as models are overfitted.
# With some random seeds, such as this one, at some point
# (in this case with K = 9) test set error starts to increase sharply. This is because 
# the model starts to overfit more heavily into the single data points; this can 
# be seen from the curves from (a) that start to fluctuate more wildly.

###################################
# (c)

# 10-fold cross-validation
k <- 10
fold_size <- n_train / k
SSE <- numeric(K+1) # sum of squared errors instead of the mean

# Training data is randomly generated, so it is also in random order; but let's choose
# folds randomly, because this is what you normally want to do anyways.
random_permutation <- sample(1:n_train)

for(i in 1:k) {
  first_idx <- (i-1) * fold_size + 1 
  last_idx <- i * fold_size
  idx_test <- random_permutation[first_idx:last_idx]
  test_data <- train[idx_test, ]
  training_data <- train[-idx_test, ]
  SSE[1] <- SSE[1] + sum((mean(training_data$y) - test_data$y)^2)
  for(i in 1:K) {
    model <- lm(y ~ poly(x,i), data = training_data)
    SSE[i+1] <- SSE[i+1] + sum((predict(model, newdata = test_data) - test_data$y)^2)
  }
}

plot(0:K, SSE, xlab = 'K', type = 'b', pch = 18, col = 'red')
SSE

# Cross-validation error decreases until K = 2, and then starts to increase. Compared on 
# the MSE on the test set, it seems to increase more clearly after the true underlying 
# polynomial decree. So it seems that it is a more reliable way to access model fit
# than using a single test set. In practice, cross-validation and validation set approaches
# can also be combined: first a model is selected using cross-validation,
# and then its fit is assessed using a held-out test set that was not used in the 
# model selection at all. 
