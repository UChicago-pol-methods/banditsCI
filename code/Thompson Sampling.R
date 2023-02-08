library(methods)
# To use setRefClass(); See http://adv-r.had.co.nz/R5.html. setRefClass() creates "reference classes". In short, reference classes allow you to create objects that behave more like objects in object-oriented programming languages (e.g. Python, Java)
library(cv.glmnet)
# Machine learning package, replacing sklearn.linear_model in python


LinTSModel <- setRefClass("LinTSModel",
                          # Linear Thompson Sampling. Computes a different model for each arm.
                          fields = list(num_mc = "numeric",
                                        K = "numeric", # Number of arms
                                        p = "numeric", # Dimension of contexts
                                        floor_start = "numeric", # Assignment probability floor starting point
                                        floor_decay = "numeric", # Assignment probability floor decaying rate
                                        mu = "numeric",
                                        V = "numeric",
                                        X = "list",
                                        y = "list"), 
                          methods = list(
                            initialize = function(K, p, floor_start, floor_decay, num_mc = 100) {
                              self$K <- K
                              self$p <- p
                              self$floor_start <- floor_start
                              self$floor_decay <- floor_decay
                              self$num_mc <- num_mc
                              self$mu <- matrix(0, nrow = K, ncol = p + 1)
                              self$V <- array(0, dim = c(K, p + 1, p + 1))
                              self$X <- replicate(K, list(), simplify = FALSE)
                              self$y <- replicate(K, list(), simplify = FALSE)
                            }))

update_thompson_1 = function(x, y, w, model) {
  # input: 
  model$X[[w]] <- rbind(X[[w]], x)
  model$y[[w]] <- rbind(y[[w]], y)
  regr <- cv.glmnet(x, y, alpha = 0) #(alphas=[1e-5, 1e-4,1e-3, 1e-2, 1e-1, 1.0])?)
  # We'll use this type of lamba value for prediction
  coef <- coef(regr, s = 'lambda.1se') # coefficients
  yhat <- predict(regr, s = 'lambda.1se', newx = x) # prediction
  model$mu[w, ] <- c(regr$intercept_, regr$coef)
  # intercept_  is outputs of RidgeCV in python, change name if outputs of glmnet vary
  X <- rbind(matrix(1,lenth(model$X[w]),1),model$X[w])
  B <- t(X) %*% X + regr$alpha_ * diag(model$p + 1)
  model$V[w, , ] <- mean((model$y[[w]] - yhat)^2) * solve(B)
  return(model)
}

update_thompson <- function(xs, ws, ys, model) {
  # Updates LinTS agent with newly observed data.
  # xs: covariate X_t of shape [T, p]
  # ys: potential outcomes of shape [T, K]
  for (w in 1:model$K) {
    model <- update_thompson_1(xs[ws == w], ys[ws == w], w, model)
  }
  return(model)
}

draw_thompson <- function(xs, model, start, end, current_t) {
  # Draws arms with a LinTS agent for the observed covariates.
  T <- dim(xs)[1]
  p <- dim(xs)[2]
  xt <- cbind(rep(1, T), xs)
  floor <- model$floor_start / current_t^model$floor_decay
  coeff <- array(NA, dim=c(model$K, model$num_mc, p + 1))
  for (w in 1:model$K) {
    coeff[w,,] <- mvrnorm(model$num_mc, model$mu[w,], model$V[w,,]) # random.multivariate_normal from different contexts?
  }
  draws <- t(coeff %*% t(xt))
  ps <- array(NA, dim=c(T, model$K))
  for (s in 1:T) {
    ps[s, ] <- table(which.max(draws[, , s], arr.ind=TRUE) - 1) / model$num_mc
    ps[s, ] <- apply_floor(ps[s, ], floor)
  }
  w <- sapply(1:(end - start + 1), function(t) sample(1:model$K, size=1, prob=ps[start + t - 1, ]))
  return(list(w=w, ps=ps))
}
