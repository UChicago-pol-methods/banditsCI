library(glmnet)
library(Matrix)
library(MASS)

LinTSModel <- function(K, 
                       p, 
                       floor_start, 
                       floor_decay, 
                       num_mc=100, 
                       is_contextual = TRUE
) {
  model <- list()
  model$num_mc <- num_mc
  model$K <- K
  model$p <- p
  model$floor_start <- floor_start
  model$floor_decay <- floor_decay
  model$y <- replicate(K, matrix(0, nrow=0, ncol=1))
  model$ps <- replicate(K, matrix(0, nrow=0, ncol=1))
  if (is_contextual) {
    model$mu <- matrix(0, nrow=K, ncol=p+1)
    model$V <- array(0, dim=c(K, p+1, p+1))
    model$X <- replicate(K, matrix(0, nrow=0, ncol=p))
  }
  return(model)
}

update_thompson <- function(
    ws, 
    yobs, 
    model, 
    xs = NULL,
    ps = NULL,
    balanced = FALSE
) {
  for (w in 1:model$K) {
    if (!is.null(xs)) { # contextual
      model$X[[w]] <- rbind(model$X[[w]], cbind(xs[ws == w,,drop = FALSE]))
      model$y[[w]] <- rbind(model$y[[w]], cbind(yobs[ws == w, drop = FALSE]))
      model$ps[[w]] <- c(model$ps[[w]], ps[cbind(ws == w,w)])
      regr <- cv.glmnet(model$X[[w]], model$y[[w]], alpha = 0) 
      coef <- coef(regr, s = 'lambda.1se')
      
      if(isTRUE(balanced)){
        W <- 1/model$ps[[w]] # balancing weights
        X <- model$X[[w]]
        Y <- model$y[[w]]
        n <- length(Y)
        p <- ncol(X)
        sd_y <- sqrt(var(Y)*(n-1)/n)[1,1]
        mean_x <- colMeans(X)
        sd_x <- sqrt(apply(X,2,var)*(n-1)/n)
        X_scaled <- matrix(NA, nrow = n, ncol = p)
        
        for(i in 1:p){
          X_scaled[,i] <- (X[,i] - mean_x[i])/sd_x[i]
        }
        X_scaled[is.na(X_scaled)] <- 0
        
        X_scaled_ones <- cbind(rep(1,n), X_scaled)
        
        B <- t(X_scaled_ones) %*% diag(W) %*% X_scaled_ones + regr$lambda.1se/sd_y*n * diag(x = c(0, rep(1,p)))
        coefhat <- solve(B) %*% t(X_scaled_ones) %*% diag(W) %*% Y
        coefhat_rescaled <- replace(coefhat[-1]/sd_x, sd_x==0, 0)
        coefhat <- c(coefhat[1] - crossprod(mean_x, coefhat_rescaled), 
                     coefhat_rescaled)
        
        model$mu[w,] <- coefhat
        yhat <- cbind(1, X) %*% coefhat
        
        model$V[w,,] <- array(mean((model$y[[w]] - yhat)^2* W) * solve(B))
      } else{
        X <- cbind(1, model$X[[w]])
        yhat <- predict(regr, s = 'lambda.1se', model$X[[w]])
        model$mu[w,] <- coef[, drop = TRUE] # intercept and coefficients of predictors
        B <- t(X) %*% X + regr$lambda.1se * diag(model$p + 1) 
        model$V[w,,] <- array(mean((model$y[[w]] - yhat)^2) * solve(B))
      }
    } else { # noncontextual
      model$y[[w]] <- rbind(model$y[[w]], cbind(yobs[ws == w, drop = FALSE]))
    }
  }
  return(model)
}

draw_thompson <- function(
    model, 
    start, 
    end, 
    xs = NULL
) {
  floor <- model$floor_start / (model$floor_decay * start)
  
  if(!is.null(xs)){
    # Draws arms with a LinTS agent for the observed covariates.
    A <- dim(xs)[1]
    p <- dim(xs)[2]
    ps <- array(NA, dim=c(A, model$K))
    
    xt <- cbind(rep(1, A), xs)
    coeff <- array(NA, dim=c(model$K, model$num_mc, p+1))
    for (w in 1:model$K) {
      coeff[w,,] <- mvrnorm(model$num_mc, model$mu[w,], model$V[w,,]) # random.multivariate_normal from different contexts
    }
    draws <- apply(coeff, c(1,2), function(x) {xt %*% x}) # double check this line
    
    for (s in 1:nrow(ps)) { # TODO and double check that draws is doing the right thing here
      ps[s, ] <- table(factor(apply(draws[s, , ], 2, which.max), levels = 1:model$K) ) / model$num_mc
      ps[s, ] <- impose_floor(ps[s, ], floor)
    }
    w <- sapply(1:(end - start + 1), function(t) sample(1:model$K, size=1, prob=ps[start + t - 1, ]))
    
  } else { 
    # Draws arms with a non-contextual TS agent. 
    if( all( unique(unlist(model$y)) %in% c(0,1)) ){ # bernoulli sampling
      successes <- unlist(lapply(model$y, function(x) sum(x)))
      failures <- unlist(lapply(model$y, function(x) length(x) - sum(x)))
      draws <- replicate(model$num_mc, rbeta(model$K, successes + 1, failures + 1)) # + 1 is prior
    } else { # normal approximation sampling
      muhats <- unlist(lapply(model$y, mean))
      sigmahats <- unlist(lapply(model$y, function(x) sd(x)/sqrt(length(x))))
      draws <- replicate(model$num_mc, rnorm(model$K, mean = muhats, sd = sigmahats))
    }
    argmax <- apply(draws, 2, which.max)
    ts_probs <- unname(table(factor(argmax, levels = 1:model$K)) / model$num_mc)
    ps <- impose_floor(ts_probs, floor)
    w <- sample(1:model$K, size = end - start + 1, prob = ps, replace = TRUE)
  }
  
  return(list(w=w, ps=ps))
}

run_experiment <- function(
    ys, 
    floor_start, 
    floor_decay, 
    batch_sizes, 
    xs = NULL,
    balanced = NULL
) {
  # Run bandit experiment
  # INPUT:
  # - xs: covariate X_t of shape [A, p]
  # - ys: potential outcomes of shape [A, K]
  # OUTPUT:
  # - pulled arms, observed rewards, assignment probabilities
  A <- dim(ys)[1] # A: the number of observations 
  K <- dim(ys)[2] # K: the number of arms
  ws <- numeric(A) # the index of the selected arm. The ws array is a 1-dimensional array.
  yobs <- numeric(A)
  
  
  probs <- array(0, dim = c(A, A, K))
  Probs_t <- matrix(0, A, K)
  p <- dim(xs)[2]
  
  bandit_model <- LinTSModel(p = p, 
                             K = K, 
                             floor_start = floor_start, 
                             floor_decay = floor_decay, 
                             is_contextual = !is.null(xs)
  )
  
  # uniform sampling at the first batch
  batch_size_cumsum <- cumsum(batch_sizes) # 
  ws[1:batch_size_cumsum[1]] <- sample(1:K, batch_size_cumsum[1], replace = TRUE) # TODO: make complete RA
  yobs[1:batch_size_cumsum[1]] <- ys[cbind(1:batch_size_cumsum[1], ws[1:batch_size_cumsum[1]])]
  probs[1:batch_size_cumsum[1], , ] <- array(1/K, dim = c(batch_size_cumsum[1], A, K))
  
  if(!is.null(xs)){ # contextual case
    bandit_model <- update_thompson(ws = ws[1:batch_size_cumsum[1]], 
                                    yobs = yobs[1:batch_size_cumsum[1]], 
                                    model = bandit_model,
                                    xs = xs[1:batch_size_cumsum[1], ],
                                    ps = matrix(1/K, nrow = A, ncol = K)[1:batch_size_cumsum[1], ],
                                    balanced = balanced)
  } else { # non-contextual case
    bandit_model <- update_thompson(ws = ws[1:batch_size_cumsum[1]], 
                                    yobs = yobs[1:batch_size_cumsum[1]], 
                                    model = bandit_model)
  }
  
  
  # adaptive sampling at the subsequent batches
  for (idx in 1:(length(batch_sizes)-1)) {
    ff <- batch_size_cumsum[idx] + 1
    l <- batch_size_cumsum[idx + 1] 
    draw <- draw_thompson(model = bandit_model, start=ff, end=l, xs = xs)
    w <- draw$w
    ps <- draw$ps
    yobs[ff:l] <- ys[cbind(ff:l, w)]
    ws[ff:l] <- w
    probs[ff:l, , ] <- array(replicate(A, ps), dim = c(l-ff+1, A, K))
    # probs[ff:l, , ] <- aperm(sapply(ff:l, function(x) ps, simplify = "array"), c(3,1,2))
    
    if(!is.null(xs)){ # contextual case
      bandit_model <- update_thompson(ws = ws[ff:l], 
                                      yobs = yobs[ff:l], 
                                      model = bandit_model,
                                      xs = xs[ff:l,],
                                      ps = ps[ff:l,],
                                      balanced = balanced)
    } else { # non-contextual case
      bandit_model <- update_thompson(ws = ws[ff:l], 
                                      yobs = yobs[ff:l], 
                                      model = bandit_model)
    }
  }
  # probs are assignment probabilities e_t(X_s, w) of shape [A, A, K]
  if(is.null(xs)){ bandit_model <- NULL }
  
  data <- list(yobs = yobs, ws = ws, xs = xs, ys = ys, probs = probs, fitted_bandit_model = bandit_model)
  
  return(data)
}

impose_floor <- function(
    a, 
    amin
) {
  new <- pmax(a, amin)
  total_slack <- sum(new) - 1
  individual_slack <- new - amin
  c <- total_slack / sum(individual_slack)
  new <- new - c * individual_slack
  return(new)
}

generate_bandit_data <- function(X=NULL, 
                                 y=NULL, 
                                 noise_std=1.0, 
                                 signal_strength=1.0) {
  # Generate covariates and potential outcomes from a classification dataset.
  
  shuffler <- sample(1:nrow(X))
  xs <- X[shuffler,]
  ys <- y[shuffler]
  A <- nrow(xs)
  A <- min(A, 20000)
  xs <- xs[1:A,]
  ys <- ys[1:A]
  K <- length(unique(ys))
  muxs <- matrix(0, nrow=A, ncol=K)
  for (k in 1:K) {
    muxs[,k] <- as.integer(ys == unique(ys)[k]) * signal_strength
  }
  ys <- muxs + rnorm(n=A*K, mean=0, sd=noise_std)
  mus <- table(y) / A
  data <- list(xs=xs, ys=ys, muxs=muxs, A=A, p=ncol(xs), K=K)
  return(list(data = data, mus = mus))
}

# balwts: inverse probability score 1[W_t=w]/e_t(w) of pulling arms, shape [A, K]
balwts <- function(ws, probs) {
  A <- length(ws)
  if (length(dim(probs)) == 2) {
    K <- dim(probs)[2]
    balwts <- matrix(0, nrow = A, ncol = K)
    for (a in 1:A) {
      balwts[a, ws[a]] <- 1/probs[a, ws[a]]
    }
  } else {
    K <- dim(probs)[3]
    balwts <- array(0, dim = c(A, K))
    for (a in 1:A) {
      balwts[a, ] <- 1/probs[a, a, ]
    }
  }
  return(balwts)
}

calculate_mu_hat <- function(results) {
  if (!is.null(results$fitted_bandit_model)) {
    # Contextual Thompson Sampling
    mu_transpose <- t(results$fitted_bandit_model$mu)
    xs_1 <- cbind(1, results$xs)
    mu_hat <- xs_1 %*% mu_transpose
    return(mu_hat)
  }
}

# generate plot_cumulative_assignment function
plot_cumulative_assignment <- function(
    results, 
    batch_sizes
) {
  # Plot the cumulative assignment graph for every arm and every batch size, x-axis is the number of observations, y-axis is the cumulative assignment
  # INPUT:
  # - results: results from run_experiment function
  # - batch_sizes: batch sizes used in the experiment
  # OUTPUT:
  # - plot of cumulative assignment graph
  
  # access dataset components
  ws <- results$ws
  A <- length(ws)
  K <- dim(results$probs)[length(dim(results$probs))]
  batch_size_cumsum <- cumsum(batch_sizes)
  
  dat <- matrix(0, nrow = A, ncol = K)
  dat[cbind(1:A, ws)] <- 1 
  dat <- apply(dat, 2, cumsum) 
  matplot(dat, type = c("l"), col =1:K) 
  abline(v=batch_size_cumsum, col="#00ccff")
  legend("topleft", legend = 1:K, col=1:K, lty=1:K)
}

