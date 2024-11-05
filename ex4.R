# IML 2018
# Exercise set 3
setwd("~/R")

##############################################
# Exercise 2
# a 

source('naive_bayes.R') 
#functions for generating the data, training the naive Bayes classifier, and predicting with this classifier.


# compute a negative of log likelihood (-log p(y|x) for test data y) 
logloss <- function(probs, y) {
  loss <- 0
  for(i in seq_along(y))
    loss <- loss - log2(probs[i,y[i]+1]) #base 2
  loss
}


library(nnet) #For logistic regression models produced using the function multinom

n_train <- 10000
train <- generate_data(n_train) 

n_test <- 1e4
test <- generate_data(n_test) 

# fit Naive Bayes model with smoothing parameters m = 0, 1/2, and 1
nbfit_ML <- nb(train$Y, train$X, m = 0)
nbfit_KT <- nb(train$Y, train$X, m = 1/2)
nbfit_Laplace <- nb(train$Y, train$X, m = 1)

paste('Maximum likelihood:', logloss(predict(nbfit_ML, test$X, probabilities = TRUE), test$Y))
paste('Krichesky-Trofimov: ',logloss(predict(nbfit_KT, test$X, probabilities = TRUE), test$Y))
paste('Laplace:                   ', logloss(predict(nbfit_Laplace, test$X, probabilities = TRUE), test$Y))

# try with different training set sizes
test_df <- data.frame(Y = test$Y, X1 = factor(test$X[ ,1]), X2 = factor(test$X[ ,2]))

#  different training set sizes
n_train <- c(25, 50, 100, 200, 400, 800, 1600, 3200, 6400)
test_error_ML <- test_error_KT <- test_error_Laplace <- test_error_logistic <- test_error_logistic_ia <- numeric(length(n_train))
logloss_ML <- logloss_Laplace <- logloss_KT <- logloss_logistic <- logloss_logistic_ia <- numeric(length(n_train))
for(i in seq_along(n_train)) {
  train <- generate_data(n_train[i])
  train_df <- data.frame(Y = train$Y, X1 = factor(train$X[ ,1]), X2 = factor(train$X[ ,2]))
  
  nbfit_ML <- nb(train$Y, train$X, m = 0)
  nbfit_Laplace <- nb(train$Y, train$X, m = 1)
  nbfit_KT <- nb(train$Y, train$X, m = 1/2)
  logistic_fit <- multinom(Y ~ X1 + X2, data = train_df)
  logistic_fit_ia <- multinom(Y ~ X1 * X2, data = train_df) 
  
  test_error_ML[i] <- mean(predict(nbfit_ML, test$X) != test$Y) 
  test_error_KT[i] <- mean(predict(nbfit_KT, test$X) != test$Y) 
  test_error_Laplace[i] <- mean(predict(nbfit_Laplace, test$X) != test$Y) 
  test_error_logistic[i] <- mean(predict(logistic_fit, test_df) != test$Y) 
  test_error_logistic_ia[i] <- mean(predict(logistic_fit_ia, test_df) != test$Y)
  
  logloss_ML[i] <- logloss(predict(nbfit_ML, test$X, probabilities = TRUE), test$Y)
  logloss_Laplace[i] <- logloss(predict(nbfit_Laplace, test$X, probabilities = TRUE), test$Y)
  logloss_KT[i] <- logloss(predict(nbfit_KT, test$X, probabilities = TRUE), test$Y)
  logloss_logistic[i] <- logloss(predict(logistic_fit, test_df, type = 'probs'), test$Y)
  logloss_logistic_ia[i] <- logloss(predict(logistic_fit_ia, test_df, type = 'probs'), test$Y)
}
# plot log loss 
plot(x = log(n_train, base = 2), logloss_logistic, type = 'b', col = 'red', lwd = 2, pch = 20,
     xlab = 'log(n)', ylab = 'log loss', ylim = c(14000, 20000))
lines(x = log(n_train, base = 2), logloss_ML, type = 'b', col = 'blue', lwd = 2, pch = 20)
lines(x = log(n_train, base = 2), logloss_KT, type = 'b', col = 'orange', lwd = 2, pch = 20)
lines(x = log(n_train, base = 2), logloss_Laplace, type = 'b', col = 'purple', lwd = 2, pch = 20)


legend("topright", legend = c('Naive Bayes (ML)', 'Naive Bayes (KT)', 'Naive Bayes (Laplace)', 'Logistic reg.'),
       inset=.05, col = c('blue', 'orange', 'purple', 'red'), lwd = c(2,2,2,2), lty = c(1,1,1,1))


# b
learn_bayes_classifier <- function(normalize = TRUE) {
  X_comb <- expand.grid(0:1,0:2)
  likelihood <- matrix(0, ncol = 3, nrow = 6)
  likelihood[, 1]  <-  c(0.2,0.1,0.4,0.2,0.0,0.1)
  likelihood[, 2] <-  c(0.6,0.1,0.1,0.1,0.1,0.0)
  likelihood[, 3] <-  c(0.1,0.4,0.3,0.0,0.2,0.0)
  priori <- c(.4, .3, .3)
  joint <- t(apply(likelihood, 1, function(row) row * priori))
  ret <- if(normalize) joint / rowSums(joint) else joint
  class(ret) <- 'bayes'
  ret
}
predict.bayes <- function(classifier, data, probabilities = FALSE) {
  n <- nrow(data)
  X_comb <- expand.grid(0:1,0:2)
  d <- nrow(X_comb)
  probs <- matrix(0, ncol = 3, nrow = n)
  for(i in 1:nrow(data)) 
    for(j in 1:d) 
      if(data[i,1] == X_comb[j,1] & data[i,2] == X_comb[j,2]) {
        probs[i, ] <- classifier[j, ]
        break
      }
  probs <- probs / rowSums(probs)      # normalize into conditional probabilities p(y|x) 
  pred <- max.col(probs) - 1        # predict class y as argmax p(y|x)
  if(probabilities) probs else pred     
}
bayes_true <- learn_bayes_classifier()
pred_bayes <- predict(bayes_true, test_df[ ,2:3])
bayes_error <- mean(pred_bayes != test$Y)
logloss_bayes <- logloss(predict(bayes_true, test_df[ ,2:3], probabilities = TRUE), test$Y)

paste('Misclassification rate of Bayes-optimal classifier: ', round(bayes_error, 4))

# c

learn_bayes_classifier(normalize = FALSE)
expand.grid(0:1, 0:2) # get the order of the values of X in the table

# d



# Exercise 3
# a
library(MASS)
n_sim <- 200
Y <- c(rep(-1, n_sim), rep(1, n_sim)) # set class labels

Sigma_plus <- 16*diag(2)
Sigma_minus <- diag(2)
mu_plus <- c(0,0)
mu_minus <- c(0,0)
Xplus <- mvrnorm(n_sim, mu = mu_plus, Sigma_plus)
Xminus <- mvrnorm(n_sim, mu = mu_minus, Sigma_minus)
train <- data.frame(Y = factor(Y), X1 = c(Xminus[ ,1], Xplus[ ,1]), X2 = c(Xminus[ ,2], Xplus[ ,2]))

plot(train$X1[Y==1],train$X2[Y==1], asp = 1, col = 'red', pch=20)
lines(train$X1[Y==-1],train$X2[Y==-1], asp = 1, col = 'green', pch=20, type = 'p')

library(e1071)


ranges <- list(cost = c(.001, .1, 1, 100))
cv_linear <- tune(svm, Y ~ ., data = train, kernel = 'linear', scale = FALSE, ranges = ranges)
summary(cv_linear)



svmfit <- svm(Y ~ ., data = train, kernel = 'linear', cost = 10, scale = FALSE)
summary(svmfit)
plot(svmfit, train, asp = 1)
mean(predict(svmfit) != train$Y) 



svmfit_poly <- svm(Y ~ X1 + X2, data = train, kernel = 'polynomial', degree = 2, cost = 1, scale = FALSE)
summary(svmfit_poly) 
plot(svmfit_poly, train, asp = 1)
mean(predict(svmfit_poly) != train$Y) 


ranges <- list(cost = c(.001, .1, 1, 100), gamma = c(1,2,4))
cv_radial <- tune(svm, Y ~ ., data = train, kernel = 'radial', scale = FALSE, ranges = ranges)
summary(cv_radial)


svmfit_radial <- svm(Y ~ ., data = train, kernel = 'radial', cost = 1, gamma = 1, scale = FALSE)
summary(svmfit_radial) 
plot(svmfit_radial, train, asp = 1)
mean(predict(svmfit_radial) != train$Y)


train$X3 <- train$X1^2
train$X4 <- train$X2^2


ranges <- list(cost = c(.001, .1, 1, 100))
cv_linear <- tune(svm, Y ~ ., data = train, kernel = 'linear', scale = FALSE, ranges = ranges)
summary(cv_linear)



train2 <- data.frame(Y = train$Y, X3 = train$X1^2, X4 = train$X2^2)
svmfit <- svm(Y ~ X3 + X4, data = train2, kernel = 'linear', cost = 10, scale = FALSE)
summary(svmfit) #
plot(svmfit, train2, asp = 1, xlim = c(0,25), ylim = c(0,25))
mean(predict(svmfit) != train$Y) 


ranges <- list(cost = c(.001, .1, 1, 100), gamma = c(1,2,4))
cv_radial <- tune(svm, Y ~ ., data = train, kernel = 'radial', scale = FALSE, ranges = ranges)
summary(cv_radial)


# Exercise 3 b

# a
library(ISLR)
n <- nrow(OJ)
n_train <- 800
idx_train <- sample(1:n, 800) 
OJ_train <- OJ[idx_train, ]
OJ_test <- OJ[-idx_train, ]

# b
svmfit <- svm(Purchase ~., data = OJ_train, scale = TRUE, cost = .01, kernel = 'linear')
summary(svmfit)


# c
mean(predict(svmfit) != OJ_train$Purchase) 
mean(predict(svmfit, OJ_test) != OJ_test$Purchase) 

# d
ranges <- list(cost = c(.01, .1, 1, 5, 10)) 
cv_linear <- tune(svm, Purchase ~ ., data = OJ_train, kernel = 'linear', scale = TRUE, ranges = ranges)
summary(cv_linear)


svmfit <- svm(Purchase ~., data = OJ_train, scale = TRUE, cost = 1, kernel = 'linear')
mean(predict(svmfit) != OJ_train$Purchase)
mean(predict(svmfit, OJ_test) != OJ_test$Purchase) 

