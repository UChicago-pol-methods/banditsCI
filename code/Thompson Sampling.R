library(glmnet)
# Machine learning package, replacing sklearn.linear_model in python

LinTSModel <- function(K, p, floor_start, floor_decay, num_mc=100) {
  model <- list()
  model$num_mc <- num_mc
  model$K <- K
  model$p <- p
  model$floor_start <- floor_start
  model$floor_decay <- floor_decay
  model$mu <- matrix(0, nrow=K, ncol=p+1)
  model$V <- array(0, dim=c(K, p+1, p+1))
  model$X <- vector("list", K)
  model$y <- vector("list", K)
  return(model)
}

update_thompson_1 = function(x, y, w, model) {
  # input: 
  model$X[[w]] <- rbind(model$X[[w]], x)
  model$y[[w]] <- rbind(model$y[[w]], y)
  regr <- cv.glmnet(model$X[[w]], model$y[[w]])             ### ERROR 
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
  # xs: covariate X_t of shape [A, p]
  # ys: potential outcomes of shape [A, K]
  for (w in 1:model$K) {
    model <- update_thompson_1(xs[ws == w], ys[ws == w], w, model) ### DISCUSS: might be something wrong with xs.
  }
  return(model)
}

draw_thompson <- function(xs, model, start, end, current_t) {
  # Draws arms with a LinTS agent for the observed covariates.
  A <- dim(xs)[1]
  p <- dim(xs)[2]
  xt <- cbind(rep(1, A), xs)
  floor <- model$floor_start / current_t^model$floor_decay
  coeff <- array(NA, dim=c(model$K, model$num_mc, p + 1))
  for (w in 1:model$K) {
    coeff[w,,] <- mvrnorm(model$num_mc, model$mu[w,], model$V[w,,]) # random.multivariate_normal from different contexts
  }
  draws <- t(coeff %*% t(xt))
  ps <- array(NA, dim=c(A, model$K))
  for (s in 1:A) {
    ps[s, ] <- table(which.max(draws[, , s], arr.ind=TRUE) - 1) / model$num_mc
    ps[s, ] <- apply_floor(ps[s, ], floor)
  }
  w <- sapply(1:(end - start + 1), function(t) sample(1:model$K, size=1, prob=ps[start + t - 1, ]))
  return(list(w=w, ps=ps))
}
