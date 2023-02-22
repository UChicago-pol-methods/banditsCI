
library(Matrix)
# Functions
source('Thompson Sampling.R')

run_experiment <- function(xs, ys, config, batch_sizes) {
  # Run contextual bandit experiment
  # INPUT:
  # - xs: covariate X_t of shape [T, p]
  # - ys: potential outcomes of shape [T, K]
  # - config: dictionary of experiment configs #DISCUSS?
  # - batch_sizes: nparray of batch sizes ?????
  # OUTPUT:
  # - pulled arms, observed rewards, assignment probabilities
  T <- dim(ys)[1] # T: the number of observations #TODO
  K <- dim(ys)[2] # K: the number of arms
  p <- dim(xs)[2]
  ws <- numeric(T) # the index of the selected arm? length = T. The ws array is a 1-dimensional array that will store the action
  yobs <- numeric(T)
  probs <- matrix(0, T, T, K)
  Probs_t <- matrix(0, T, K)
  floor_start <- config$floor_start #DISCUSS?
  floor_decay <- config$floor_decay
  
  if (config$bandit_model == "TSModel") {
    bandit_model <- LinTSModel(K = K, p = p, floor_start = floor_start, floor_decay = floor_decay)
    draw_model <- draw_thompson
  }
  
  # uniform sampling at the first batch
  batch_size_cumsum <- cumsum(batch_sizes) # what is batch_sizes? how to input batch_sizes?
  ws[1:batch_size_cumsum[1]] <- rep(1:batch_size_cumsum[1] %% K, each = 1)  # batch_size_cumsum[1] = number of obs ? DISCUSS?
  yobs[1:batch_size_cumsum[1]] <- ys[cbind(1:batch_size_cumsum[1], ws[1:batch_size_cumsum[1]])]
  probs[1:batch_size_cumsum[1], , ] <- array(1/K, dim = c(batch_size_cumsum[1], T, K))
  Probs_t[1:batch_size_cumsum[1], ] <- matrix(1/K, batch_size_cumsum[1], K)
  if (config$bandit_model == "TSModel") {
    bandit_model <- update_thompson(xs[1:batch_size_cumsum[1], ], 
                                    ws[1:batch_size_cumsum[1]], 
                                    yobs[1:batch_size_cumsum[1]], 
                                    bandit_model)
  }
  
  # adaptive sampling at the subsequent batches
  for (idx in 1:(length(batch_sizes)-1)) {
    f <- batch_size_cumsum[idx] 
    l <- batch_size_cumsum[idx + 1] # is the index identical with python code? DISCUSS?
    if (config$bandit_model == "TSModel") {
      draw <- draw_thompson(xs, bandit_model, start=f, end=l, current_t = f)
    }
    w <- draw$w
    p <- draw$p
    yobs[f:l] <- ys[cbind(f:l, w)]
    ws[f:l] <- w
    probs[f:l, , ] <- array(p, dim = c(l - f, T, K))
    Probs_t[f:l, ] <- p[f:l, ]
    if (config$bandit_model == "TSModel") {
      bandit_model <- update_thompson(
      xs[f:l], ws[f:l], yobs[f:l], bandit_model)
    }
  }
  data <- list(yobs = yobs, ws = ws, xs = xs, ys = ys, probs = probs, fitted_bandit_model = bandit_model)
  
  return(data)
}
