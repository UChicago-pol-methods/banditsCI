#' LinTSModel Function
#'
#' The LinTSModel function creates a linear Thompson Sampling model for multi-armed bandit problems.
#'
#' @param K the number of arms in the bandit problem
#' @param p the dimension of the contextual vector, if is_contextual is set to TRUE. Otherwise, p is ignored
#' @param floor_start the initial floor value for the exploration parameter
#' @param floor_decay the decay rate of the floor value for the exploration parameter
#' @param num_mc the number of Monte Carlo simulations used to approximate the expected reward
#' @param is_contextual a logical value indicating whether the problem is contextual or not
#'
#' @return A list containing the parameters of the LinTSModel
#'
#' @examples
#' model <- LinTSModel(K=5, p=3, floor_start=1, floor_decay=0.9, num_mc=100, is_contextual=TRUE)
#'
#' @export
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

#' update_thompson Function
#'
#' The update_thompson function updates the parameters of a linear Thompson Sampling model for multi-armed bandit problems based on new observations.
#'
#' @param ws a vector of integers indicating which arm was chosen for each observation
#' @param yobs a vector of numeric values representing the observed rewards
#' @param model a list containing the parameters of the LinTSModel
#' @param xs an optional matrix of contextual vectors for the observations, if the LinTSModel is contextual
#' @param ps an optional matrix of probabilities of selecting each arm for each observation, if the LinTSModel is balanced
#' @param balanced a logical value indicating whether to use balanced Thompson Sampling or not
#'
#' @return A list containing the updated parameters of the LinTSModel
#'
#' @examples
#' model <- LinTSModel(K=5, p=3, floor_start=1, floor_decay=0.9, num_mc=100, is_contextual=TRUE)
#' model <- update_thompson(ws=c(3,2,1), yobs=c(1,0,1), model=model, xs=matrix(rnorm(9), ncol=3))
#'
#' @export
update_thompson <- function(
    ws,
    yobs,
    model,
    xs = NULL,
    ps = NULL,
    balanced = NULL
) {
  for (w in 1:model$K) {
    if (!is.null(xs)) { # contextual
      model$X[[w]] <- rbind(model$X[[w]], cbind(xs[ws == w,,drop = FALSE]))
      model$y[[w]] <- rbind(model$y[[w]], cbind(yobs[ws == w, drop = FALSE]))
      model$ps[[w]] <- c(model$ps[[w]], ps[cbind(ws == w,w)])
      regr <- glmnet::cv.glmnet(model$X[[w]], model$y[[w]], alpha = 0)
      coef <- glmnet::coef.glmnet(regr, s = 'lambda.1se')

      if(isTRUE(balanced)){
        W <- 1/model$ps[[w]] # balancing weights
        X <- model$X[[w]]
        Y <- model$y[[w]]
        n <- length(Y)
        p <- ncol(X)
        sd_y <- sqrt(stats::var(Y)*(n-1)/n)[1,1]
        mean_x <- colMeans(X)
        sd_x <- sqrt(apply(X,2,stats::var)*(n-1)/n)
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
        yhat <- stats::predict(regr, s = 'lambda.1se', model$X[[w]])
        model$mu[w,] <- coef[,] # intercept and coefficients of predictors
        B <- t(X) %*% X + regr$lambda.1se * diag(model$p + 1)
        model$V[w,,] <- array(mean((model$y[[w]] - yhat)^2) * solve(B))
      }
    } else { # noncontextual
      model$y[[w]] <- rbind(model$y[[w]], cbind(yobs[ws == w, drop = FALSE]))
    }
  }
  return(model)
}

#' draw_thompson Function
#'
#' The draw_thompson function draws arms from a LinTS or non-contextual TS agent for multi-armed bandit problems.
#'
#' @param model a list containing the parameters of the LinTSModel
#' @param start an integer indicating the starting index of the observations to make the draw
#' @param end an integer indicating the ending index of the observations to make the draw
#' @param xs an optional matrix of contextual vectors for the observations, if the LinTSModel is contextual
#'
#' @return A list containing the drawn arms (w) and their corresponding probabilities (ps)
#'
#' @examples
#' model <- LinTSModel(K=5, p=3, floor_start=1, floor_decay=0.9, num_mc=100, is_contextual=TRUE)
#' draws <- draw_thompson(model=model, start=1, end=10, xs=matrix(rnorm(30), ncol=3))
#'
#' @export
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
      coeff[w,,] <- MASS::mvrnorm(model$num_mc, model$mu[w,], model$V[w,,]) # random.multivariate_normal from different contexts
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
      draws <- replicate(model$num_mc, stats::rbeta(model$K, successes + 1, failures + 1)) # + 1 is prior
    } else { # normal approximation sampling
      muhats <- unlist(lapply(model$y, mean))
      sigmahats <- unlist(lapply(model$y, function(x) stats::sd(x)/sqrt(length(x))))
      draws <- replicate(model$num_mc, stats::rnorm(model$K, mean = muhats, sd = sigmahats))
    }
    argmax <- apply(draws, 2, which.max)
    ts_probs <- unname(table(factor(argmax, levels = 1:model$K)) / model$num_mc)
    ps <- impose_floor(ts_probs, floor)
    w <- sample(1:model$K, size = end - start + 1, prob = ps, replace = TRUE)
  }

  return(list(w=w, ps=ps))
}

#' run_experiment Function
#'
#' The run_experiment function runs a LinTS or non-contextual TS bandit experiment, given potential outcomes and covariates.
#'
#' @param ys a matrix of potential outcomes of shape [A, K], where A is the number of observations and K is the number of arms
#' @param floor_start a numeric value indicating the starting value of the floor
#' @param floor_decay a numeric value indicating the decay rate of the floor
#' @param batch_sizes a vector of integers indicating the size of each batch
#' @param xs an optional matrix of contextual vectors for the observations, if the LinTSModel is contextual
#' @param balanced an optional boolean value indicating whether to balance the batches
#'
#' @return A list containing the pulled arms (ws), observed rewards (yobs), assignment probabilities (probs), and the fitted bandit model (fitted_bandit_model)
#'
#' @examples
#' ys <- matrix(rbinom(1000, 1, 0.5), ncol=5)
#' xs <- matrix(rnorm(300), ncol=3)
#' batch_sizes <- c(100, 200, 700)
#' result <- run_experiment(ys=ys, floor_start=1, floor_decay=0.9, batch_sizes=batch_sizes, xs=xs, balanced=TRUE)
#'
#' @export
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
    probs[ff:l, , ] <- aperm(
      array(matrix(ps, nrow = A, ncol = K, byrow = is.null(xs)),
            dim = dim(probs[ff:l, , ])[c(2,3,1)]),
      c(3,1,2))

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

#' impose_floor Function
#'
#' The impose_floor function imposes a floor on the given array a, ensuring that its elements are greater than or equal to amin.
#'
#' @param a a numeric vector to impose a floor on
#' @param amin a numeric value indicating the minimum allowed value
#'
#' @return A numeric vector with the same length as a, with the floor imposed on its elements
#'
#' @examples
#' a <- c(0.25, 0.25, 0.25, 0.25)
#' imposed_a <- impose_floor(a=a, amin=0.1)
#'
#' @export
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

#' generate_bandit_data Function
#'
#' The generate_bandit_data function generates covariates and potential outcomes from a classification dataset.
#'
#' @param X an optional matrix of covariates of shape [N, p], where N is the number of observations and p is the number of features
#' @param y an optional vector of labels of length N
#' @param noise_std a numeric value indicating the standard deviation of the noise added to the potential outcomes
#' @param signal_strength a numeric value indicating the strength of the signal in the potential outcomes
#'
#' @return A list containing the generated data (xs, ys, muxs, A, p, K) and the true class probabilities (mus)
#'
#' @examples
#' data <- generate_bandit_data(X=iris[,1:4], y=iris[,5], noise_std=0.1, signal_strength=1.0)
#' xs <- data$data$xs
#' ys <- data$data$ys
#'
#' @export
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
    muxs[,k] <- as.integer(ys == k) * signal_strength
  }
  ys <- muxs + stats::rnorm(n=A*K, mean=0, sd=noise_std)
  mus <- table(y) / A
  data <- list(xs=xs, ys=ys, muxs=muxs, A=A, p=ncol(xs), K=K)
  return(list(data = data, mus = mus))
}

simple_tree_data <- function(A, K=5, p=10, noise_std=1.0, split=1.676, signal_strength=1.0, seed=NULL, noise_form='normal') {
  # Generate covariates and potential outcomes of a synthetic dataset.

  stopifnot(p >= 2) # to check the input parameters satisfy certain conditions
  stopifnot(K >= 4)
  stopifnot(split >= 0)

  set.seed(seed)
  # Generate experimental data
  xs <- matrix(stats::rnorm(A*p), ncol=p)

  r0 <- (xs[,1] < split) & (xs[,2] < split)
  r1 <- (xs[,1] < split) & (xs[,2] > split)
  r2 <- (xs[,1] > split) & (xs[,2] < split)
  r3 <- (xs[,1] > split) & (xs[,2] > split)

  wxs <- matrix(0, nrow=A, ncol=K)
  wxs[r0,1] <- 1
  wxs[r1,2] <- 1
  wxs[r2,3] <- 1
  wxs[r3,4] <- 1
  muxs <- wxs * signal_strength
  if (noise_form == 'normal') {
    ys <- muxs + matrix(stats::rnorm(A*K, mean=0, sd=noise_std), ncol=K)
  } else {
    ys <- muxs + matrix(stats::runif(A*K, min=-noise_std, max=noise_std), ncol=K)
  }

  mvn <- mvtnorm::pmvnorm(upper=c(split, split), mean=rep(0,2), corr=diag(2))
  mus <- rep(0, K)
  mus[1] <- mvn
  mus[2] <- mvtnorm::pmvnorm(lower=c(-Inf, split), upper=c(split, Inf), mean=rep(0,2), corr=diag(2))
  mus[3] <- mvtnorm::pmvnorm(lower=c(split, -Inf), upper=c(Inf, split), mean=rep(0,2), corr=diag(2))
  mus[4] <- mvtnorm::pmvnorm(lower=c(-Inf, -Inf), upper=c(-split, -split), mean=rep(0,2), corr=diag(2))
  mus <- mus * signal_strength

  data <- list(xs=xs, ys=ys, muxs=muxs, wxs=wxs)

  return(list(data = data, mus = mus))
}

#' calculate_balwts Function
#'
#' The calculate_balwts function calculates the inverse probability score of pulling arms, given the actions taken and the true probabilities of each arm being chosen.
#'
#' @param ws a vector of length A containing the actions taken at each time step
#' @param probs a matrix of shape [A, K] or [A, A, K] containing the true probabilities of each arm being chosen at each time step
#'
#' @return A matrix of shape [A, K] or [A, A, K] containing the inverse probability score of pulling arms
#'
#' @examples
#' A <- 5
#' K <- 3
#' ws <- sample(1:K, A, replace = TRUE)
#' probs <- matrix(runif(A * K), nrow = A, ncol = K)
#' balwts <- calculate_balwts(ws, probs)
#'
#' @export
calculate_balwts <- function(ws, probs) {
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

#' calculate_mu_hat Function
#'
#' The calculate_mu_hat function calculates the estimated expected value of the reward for each arm, given the results of a bandit model.
#'
#' @param results a list containing the results of a bandit model, including the fitted model and the covariate matrix xs
#'
#' @return A vector containing the estimated expected value of the reward for each arm
#'
#' @examples
#' A <- 200
#' K <- p <- 3
#' xs <- matrix(runif(A * p), nrow = A, ncol = p)
#' ys <- matrix(rbinom(A * K, 1, 0.5), nrow = A, ncol = K)
#' batch_sizes <- c(100,100)
#' results <- run_experiment(ys = ys, floor_start = 5, floor_decay = 0.9, batch_sizes = batch_sizes, xs = xs)
#' mu_hat <- calculate_mu_hat(results)
#'
#' @export
calculate_mu_hat <- function(results) {
  if (!is.null(results$fitted_bandit_model)) {
    # Contextual Thompson Sampling
    A <- length(results$yobs)
    K <- results$fitted_bandit_model$K
    mu_hat <- matrix(NA, nrow = A, ncol = K)
    X <- results$xs
    for(w in 1:results$fitted_bandit_model$K){
      coefhat <- results$fitted_bandit_model$mu[w,]
      mu_hat[,w] <- cbind(1, X) %*% coefhat
    }
    return(mu_hat)
  }
}

#' plot_cumulative_assignment Function
#'
#' The plot_cumulative_assignment function generates a plot of the cumulative assignment graph for every arm and every batch size.
#'
#' @param results a list containing the results of the experiment, including the actions taken (ws) and the true probabilities of each arm being chosen (probs)
#' @param batch_sizes a vector of batch sizes used in the experiment
#'
#' @return A plot of the cumulative assignment graph
#'
#' @examples
#' A <- 5
#' K <- 3
#' xs <- matrix(runif(A * K), nrow = A, ncol = K)
#' ys <- matrix(rbinom(A * K, 1, 0.5), nrow = A, ncol = K)
#' model <- bandit_lin_ucb(xs, ys)
#' experiment <- run_experiment(model, batch_sizes = c(10, 20, 30))
#' plot_cumulative_assignment(experiment$results, experiment$batch_sizes)
#'
#' @export
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
  graphics::matplot(dat, type = c("l"), col =1:K)
  graphics::abline(v=batch_size_cumsum, col="#00ccff")
  graphics::legend("topleft", legend = 1:K, col=1:K, lty=1:K)
}

