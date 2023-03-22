library(glmnet)
library(Matrix)
library(MASS)
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


update_thompson <- function(xs, ws, yobs, model) {
  # Updates LinTS agent with newly observed data.
  # xs: covariate X_t of shape [A, p]
  # ys: potential outcomes of shape [A, K]
  for (w in 1:model$K) {
    # input: 
    model$X[[w]] <- rbind(model$X[[w]], xs[ws == w,])
    model$y[[w]] <- cbind(model$y[[w]], matrix(yobs[ws == w], ncol = 1))
    regr <- cv.glmnet(model$X[[w]], model$y[[w]]) 
    # We'll use this type of lamba value for prediction
    coef <- coef(regr, s = 'lambda.1se') # coefficients
    yhat <- predict(regr, s = 'lambda.1se', newx = xs[ws == w,]) # prediction
    model$mu[w, ] <- coef(regr)[,1]
    X <- cbind(1,model$X[[w]])
    B <- t(X) %*% X + regr$lambda.min * diag(model$p + 1)
    model$V[w, , ] <- mean((model$y[[w]] - yhat)^2) * solve(B)
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
  
  draws <- aperm(apply(coeff, c(1,2), function(x) {x %*% t(xt)} ), c(2,3,1))
  
  ps <- array(NA, dim=c(A, model$K))
  for (s in 1:A) {
    ps[s, ] <- table(factor(apply(draws[, , s], 2, which.max), levels = 1:model$K) ) / model$num_mc
    ps[s, ] <- impose_floor(ps[s, ], floor)
  }
  w <- sapply(1:(end - start + 1), function(t) sample(1:model$K, size=1, prob=ps[start + t - 1, ]))
  return(list(w=w, ps=ps))
}

run_experiment <- function(xs, ys, floor_start, floor_decay, batch_sizes) {
  # Run contextual bandit experiment
  # INPUT:
  # - xs: covariate X_t of shape [A, p]
  # - ys: potential outcomes of shape [A, K]
  # OUTPUT:
  # - pulled arms, observed rewards, assignment probabilities
  A <- dim(ys)[1] # A: the number of observations 
  K <- dim(ys)[2] # K: the number of arms
  p <- dim(xs)[2]
  ws <- numeric(A) # the index of the selected arm. The ws array is a 1-dimensional array.
  yobs <- numeric(A)
  probs <- array(0, dim = c(A, A, K))
  Probs_t <- matrix(0, A, K)
  
  bandit_model <- LinTSModel(K = K, p = p, floor_start = floor_start, floor_decay = floor_decay)
  draw_model <- draw_thompson
  
  # uniform sampling at the first batch
  batch_size_cumsum <- cumsum(batch_sizes) # 
  ws[1:batch_size_cumsum[1]] <- sample(1:batch_size_cumsum[1] %% K)+1  
  yobs[1:batch_size_cumsum[1]] <- ys[cbind(1:batch_size_cumsum[1], ws[1:batch_size_cumsum[1]])]
  probs[1:batch_size_cumsum[1], , ] <- array(1/K, dim = c(batch_size_cumsum[1], A, K))
  Probs_t[1:batch_size_cumsum[1], ] <- matrix(1/K, batch_size_cumsum[1], K)
  
  bandit_model <- update_thompson(xs[1:batch_size_cumsum[1], ], 
                                  ws[1:batch_size_cumsum[1]], 
                                  yobs[1:batch_size_cumsum[1]], 
                                  bandit_model)
  
  # adaptive sampling at the subsequent batches
  for (idx in 1:(length(batch_sizes)-1)) {
    f <- batch_size_cumsum[idx] + 1
    l <- batch_size_cumsum[idx + 1] 
    draw <- draw_thompson(xs, bandit_model, start=f, end=l, current_t = f)
    w <- draw$w
    p <- draw$p
    yobs[f:l] <- ys[cbind(f:l, w)]
    ws[f:l] <- w
    probs[f:l, , ] <- array(p, dim = c(l - f + 1, A, K))
    Probs_t[f:l, ] <- p[f:l, ]
    bandit_model <- update_thompson(
      xs[f:l,], ws[f:l], yobs[f:l], bandit_model)
  }
  # probs are assignment probabilities e_t(X_s, w) of shape [A, A, K]
  data <- list(yobs = yobs, ws = ws, xs = xs, ys = ys, probs = probs, fitted_bandit_model = bandit_model)
  
  return(data)
}

generate_bandit_data <- function(n, p, K, noise_std=1.0, signal_strength=1.0) {
  # Generate covariates and potential outcomes from a classification dataset.
  # INPUT:
  # - noise_std: noise standard deviation
  # - signal_strength: signal strength multiplier
  # OUTPUT:
  # - data: covariates and potential outcomes
  # - mus: arm expected reward over the covariate space
  
  X <- matrix(rnorm(n*p), ncol=p) # - X: covariates of shape [A, p]
  y <- sample(1:K, n, replace=TRUE) # - y: labels of shape [A,]
  shuffler <- sample.int(n)
  xs <- X[shuffler, ]
  ys <- y[shuffler]
  A <- nrow(xs)
  A <- min(A, 20000)
  xs <- xs[1:A, ]
  ys <- ys[1:A]
  muxs <- matrix(0, nrow=A, ncol=K)
  muxs[cbind(1:A, as.numeric(ys))] <- signal_strength
  ys <- muxs + rnorm(A*K, 0, noise_std)
  mus <- table(y)/A
  data <- list(xs=xs, ys=ys, muxs=muxs, A=A, p=ncol(xs), K=K)
  return(list(data, mus))
}
