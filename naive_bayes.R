# generate n_sim data points from the model given in the exercise sheet
generate_data <- function(n_sim) {
  X_combinations <- as.matrix(expand.grid(0:1,0:2))
  Y <- sample(0:2, size = n_sim, replace = TRUE, prob = c(0.4,0.3,0.3)) # generate class labels 
  X <- matrix(0, nrow = n_sim, ncol = 2)
  
  for(i in 1:n_sim) {
    prob <- switch (Y[i]+1,
                    c(0.2,0.1,0.4,0.2,0.0,0.1),
                    c(0.6,0.1,0.1,0.1,0.1,0.0),
                    c(0.1,0.4,0.3,0.0,0.2,0.0))
    X[i, ] <- X_combinations[sample(1:6, size = 1, replace = TRUE, prob = prob),]
  }
  list(X = X, Y = Y)
}

# train a Naive Bayes classifier
# Y = vector of class labels
# X = data matrix containing predictors, row = data point, col = variable
# n_class = number of different classes in Y
# n_class_x = vector with numbers of different values for each predictor (cols of X)
# m = smoothing parameter
# returns an object of class 'nb' with components
#   prior = vector containing estimated probabilities for the classes
#   likelihood = list containing estimated conditional probabilities
#     for the features given class p(X_i| Y) (each component of this list 
#     contains probabilities for one variable).
#   n_class = number of different classes in Y
#   n_class_x = vector with numbers of different values for each predictor (cols of X)
nb <- function(Y, X, n_class = 3, n_class_x = c(2,3), m = 0) {
  n <- length(Y)
  likelihood <- vector('list', length = n_class)
  prior <- numeric(n_class)
  for(c in 0:(n_class-1)) {
    n_c <- sum(Y == c)
    prior[c+1] <- (n_c + m) / (n + n_class * m)
    for(i in seq_along(n_class_x)) {
      likelihood[[c+1]][[i]] <- numeric(n_class_x[i]) 
      for(j in 0:(n_class_x[i]-1)) {
        n_cj <- sum(Y == c & X[ ,i] == j)
        likelihood[[c+1]][[i]][j+1] <- (n_cj + m) / (n_c + n_class_x[i] * m)
      }
    }
  }
  ret <- list(prior = prior, likelihood = likelihood, n_class = n_class, n_class_x = n_class_x)
  class(ret) <- 'nb' 
  ret
}

# compute posterior class probabilities and predicted values using trained 
# Naive Bayes model
# nbfit = fitted Naive Bayes model, object with class 'nb'
# X = predictors as matrix, row = data point, col = variable
# return = vector with predicted values for the data points (if probabilities = FALSE)
#   or class probabilites p(y|x) (if probabilities = TRUE)
predict.nb <- function(nbfit, X, probabilities = FALSE) {
  n <- nrow(X)
  prob <- matrix(0, nrow = n, ncol = nbfit$n_class) 
  for(i in 1:n)                 # compute joint probabilities p(x,y)
    for(c in 1:nbfit$n_class) {
      prob[i,c] <- nbfit$prior[c]      
      for(j in seq_along(nbfit$n_class_x))
        prob[i,c] <- prob[i,c] * nbfit$likelihood[[c]][[j]][X[i,j] + 1]
    }
  prob <- prob / rowSums(prob)      # normalize into conditional probabilities p(y|x) 
  pred <- max.col(prob) - 1        # predict class y as argmax p(y|x)
  if(probabilities) prob else pred             
}

# true bayes optimal classifier for the data set given in the exercise sheet
# normalize : TRUE gives posterior probabilities p(y|x), FALSE gives the joint 
# probabilities p(x,y)
# return : matrix with col = value of Y, row = value of X (in the order given by expand,grid(0:1, 0:2))
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

# predict data with an bayes optimal classifier
# classifier = an object of class 'bayes' produced by the function learn_bayes_classifier
# data = data set to make a predictions on 
# probabilities = TRUE : return a matrix of probabilities with
# col : class value, row = data point,
# FALSE : return vector with the predicted values 
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

# compute a negative of log likelihood (-log p(y|x) for test data y) 
logloss <- function(probs, y) {
  loss <- 0
  for(i in seq_along(y))
    loss <- loss - log2(probs[i,y[i]+1]) #base 2
  loss
}

