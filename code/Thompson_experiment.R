
library(Matrix)
# Functions
source('Thompson Sampling.R')

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
    xs[f:l], ws[f:l], yobs[f:l], bandit_model)
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
  
# Set parameters
floor_start <- 5
floor_decay <- 0.9
batch_sizes <- c(100, 200, 300, 400)

# To generate a dataset with 1000 observations, 10 covariates, and 5 arms
set.seed(123)
data <- generate_bandit_data(n=1000, p=10, K=5, noise_std=1.0, signal_strength=1.0)

# access dataset components
xs <- data[[1]]$xs
ys <- data[[1]]$ys
muxs <- data[[1]]$muxs
A <- data[[1]]$A
p <- data[[1]]$p
K <- data[[1]]$K
mus <- data[[2]]

# Run experiment
results <- run_experiment(xs, ys, floor_start, floor_decay, batch_sizes)

