#' Linear Thompson Sampling model.
#'
#' Creates a linear Thompson Sampling model for multi-armed bandit problems.
#'
#' @param K Integer. Number of arms. Must be a positive integer.
#' @param p Integer. Dimension of the contextual vector, if \code{is_contextual} is set to \code{TRUE}. Otherwise, \code{p} is ignored. Must be a positive integer.
#' @param floor_start Numeric. Starting value of the floor. Must be a positive number.
#' @param floor_decay Numeric. Decay rate of the floor. Must be a positive number between 0 and 1.
#' @param num_mc Integer. Number of Monte Carlo simulations used to approximate the expected reward. Must be a positive integer. Default is 100.
#' @param is_contextual Logical. Indicates whether the problem is contextual or not. Default is \code{TRUE}.
#'
#' @return A list containing the parameters of the LinTSModel.
#'
#' @examples
#' model <- LinTSModel(K = 5, p = 3, floor_start = 1, floor_decay = 0.9, num_mc = 100,
#'                     is_contextual = TRUE)
#'
#' @export
LinTSModel <- function(K,
                       p = NULL,
                       floor_start,
                       floor_decay,
                       num_mc = 100,
                       is_contextual = TRUE) {
  # Input checks
  if (!is.logical(is_contextual)) stop("is_contextual must be a logical value.")
  if (is.na(is_contextual)) stop("is_contextual should not be NA.")
  if (!is.numeric(K) || (K != as.integer(K)) || K <= 0 || is.na(K)) stop("K must be a positive integer.")
  if (is_contextual) {
    if (!is.numeric(p) || (p != as.integer(p)) || p <= 0 || is.na(p)) stop("p must be a positive integer when is_contextual is TRUE.")
  } else if (!is.null(p)) {
    warning("p is ignored when is_contextual is set to FALSE.")
  }
  if (!is.numeric(floor_start) || floor_start <= 0 || is.na(floor_start)) stop("floor_start must be a positive number.")
  if (!is.numeric(floor_decay) || floor_decay <= 0 || floor_decay > 1 || is.na(floor_decay)) stop("floor_decay should be a positive numeric value between 0 and 1.")
  if (!is.numeric(num_mc) || (num_mc != as.integer(num_mc)) || num_mc <= 0 || is.na(num_mc)) stop("num_mc must be a positive integer.")

  model <- list()
  model$num_mc <- num_mc
  model$K <- K
  model$floor_start <- floor_start
  model$floor_decay <- floor_decay
  model$y <- replicate(K, matrix(0, nrow = 0, ncol = 1))
  model$ps <- replicate(K, matrix(0, nrow = 0, ncol = 1))
  if (is_contextual) {
    model$p <- p
    model$mu <- matrix(0, nrow = K, ncol = p + 1)
    model$V <- array(0, dim = c(K, p + 1, p + 1))
    model$X <- replicate(K, matrix(0, nrow = 0, ncol = p))
  }
  model$is_contextual <- is_contextual
  return(model)
}

#' Update linear Thompson Sampling model.
#'
#' Updates the parameters of a linear Thompson Sampling model for multi-armed bandit problems based on new observations.
#'
#' @param ws Integer vector. Indicates which arm was chosen for observations at each time \code{t}. Length \code{A}, where \code{A} is the number of observations. Must not contain NA values.
#' @param yobs Numeric vector. Observed outcomes, length \code{A}. Must not contain NA values.
#' @param model List. Contains the parameters of the LinTSModel.
#' @param xs Optional matrix. Covariates of shape \code{[A, p]}, where \code{p} is the number of features, if the LinTSModel is contextual. Default is \code{NULL}. Must not contain NA values.
#' @param ps Optional matrix. Probabilities of selecting each arm for each observation, if the LinTSModel is balanced. Default is \code{NULL}.
#' @param balanced Logical. Indicates whether to use balanced Thompson Sampling. Default is \code{NULL}.
#'
#' @return A list containing the updated parameters of the LinTSModel.
#'
#' @examples
#' set.seed(123)
#' model <- LinTSModel(K = 5, p = 3, floor_start = 1, floor_decay = 0.9, num_mc = 100,
#'                     is_contextual = TRUE)
#' A <- 1000
#' ws <- numeric(A)
#' yobs <- numeric(A)
#' model <- update_thompson(ws = ws, yobs = yobs, model = model)
#'
#' @export
update_thompson <- function(ws,
                            yobs,
                            model,
                            xs = NULL,
                            ps = NULL,
                            balanced = NULL) {
  # Input check
  if (!is.vector(yobs) || !is.numeric(yobs) || any(is.na(yobs))) stop("yobs must be a numeric vector without NA values.")
  if (!is.vector(ws) || length(ws) != length(yobs) || any(is.na(ws))) stop("Lengths of ws and yobs must be the same, and ws should not contain NA values.")
  if (!is.list(model) || is.null(model$K) || is.null(model$is_contextual)) stop("Invalid model.")
  if (!is.null(xs) && (!is.matrix(xs) || ncol(xs) != model$p || any(is.na(xs)))) stop("xs must be a matrix with the number of columns equal to model$p and no NA values.")
  if (!is.null(ps) && (!is.matrix(ps) || any(ps < 0) || any(ps > 1) || any(is.na(ps)))) stop("ps must be a matrix of probabilities, i.e., values between 0 and 1 without NA values.")
  if (!is.null(balanced) && (!is.logical(balanced) || any(is.na(balanced)))) stop("balanced must be a logical value without NA values.")

  for (w in 1:model$K) {
    if (!is.null(xs)) { # contextual
      model$X[[w]] <- rbind(model$X[[w]], cbind(xs[ws == w, , drop = FALSE]))
      model$y[[w]] <- rbind(model$y[[w]], cbind(yobs[ws == w, drop = FALSE]))
      model$ps[[w]] <- c(model$ps[[w]], ps[cbind(ws == w, w)])
      # if no variation in ys
      if (length(unique(model$y[[w]])) == 1) {
        regr <- stats::lm(model$y[[w]] ~ model$X[[w]])
        regr$lambda.1se = 9999
        coef <- matrix(c(unique(model$y[[w]]), rep(0, ncol(model$X[[w]]))),
                       ncol = 1)
      } else if (length(model$y[[w]]) < 10) { # no cross-validation, use largest lambda
        regr <- glmnet::glmnet(model$X[[w]], model$y[[w]], alpha = 0)
        regr$lambda.1se <- regr$lambda[1]
        coef <- glmnet::coef.glmnet(regr, s = regr$lambda[1])
      } else {
        regr <- glmnet::cv.glmnet(model$X[[w]], model$y[[w]], alpha = 0)
        coef <- glmnet::coef.glmnet(regr, s = 'lambda.1se')
      }

      if (isTRUE(balanced)) {
        W <- 1 / model$ps[[w]] # balancing weights
        X <- model$X[[w]]
        Y <- model$y[[w]]
        n <- length(Y)
        p <- ncol(X)
        sd_y <- sqrt(stats::var(Y) * (n - 1) / n)[1, 1]
        mean_x <- colMeans(X)
        sd_x <- sqrt(apply(X, 2, stats::var) * (n - 1) / n)
        X_scaled <- matrix(NA, nrow = n, ncol = p)

        for (i in 1:p) {
          X_scaled[, i] <- (X[, i] - mean_x[i]) / sd_x[i]
        }
        X_scaled[is.na(X_scaled)] <- 0

        X_scaled_ones <- cbind(rep(1, n), X_scaled)

        B <- t(X_scaled_ones) %*% diag(W) %*% X_scaled_ones + regr$lambda.1se / sd_y * n * diag(x = c(0, rep(1, p)))
        coefhat <- solve(B) %*% t(X_scaled_ones) %*% diag(W) %*% Y
        coefhat_rescaled <- replace(coefhat[-1] / sd_x, sd_x == 0, 0)
        coefhat <- c(coefhat[1] - crossprod(mean_x, coefhat_rescaled),
                     coefhat_rescaled)

        model$mu[w, ] <- coefhat
        yhat <- cbind(1, X) %*% coefhat

        model$V[w, , ] <- array(mean((model$y[[w]] - yhat)^2 * W) * solve(B))
      } else {
        X <- cbind(1, model$X[[w]])
        if (length(unique(model$y[[w]])) == 1) {
          yhat <- stats::predict(regr, as.data.frame(model$X[[w]]))
        } else {
          yhat <- stats::predict(regr, s = 'lambda.1se', model$X[[w]])
        }
        model$mu[w, ] <- coef[, ] # intercept and coefficients of predictors
        B <- t(X) %*% X + regr$lambda.1se * diag(model$p + 1)
        model$V[w, , ] <- array(mean((model$y[[w]] - yhat)^2) * solve(B))
      }
    } else { # non-contextual
      model$y[[w]] <- rbind(model$y[[w]], cbind(yobs[ws == w, drop = FALSE]))
    }
  }
  return(model)
}

#' Thompson Sampling draws.
#'
#' Draws arms from a LinTS or non-contextual TS agent for multi-armed bandit problems.
#'
#' @param model List. Contains the parameters of the model, generated by \code{LinTSModel()}.
#' @param start Integer. Starting index of observations for which arms are to be drawn. Must be a positive integer.
#' @param end Integer. Ending index of the observations for which arms are to be drawn. Must be an integer greater than or equal to \code{start}.
#' @param xs Optional matrix. Covariates of shape \code{[A, p]}, where \code{p} is the number of features, if the LinTSModel is contextual. Default is \code{NULL}. Must not contain NA values.
#'
#' @return A list containing the drawn arms (\code{w}) and their corresponding probabilities (\code{ps}).
#'
#' @examples
#' set.seed(123)
#' model <- LinTSModel(K = 5, p = 3, floor_start = 1, floor_decay = 0.9, num_mc = 100,
#'                     is_contextual = TRUE)
#' draws <- draw_thompson(model = model, start = 1, end = 10,
#'                        xs = matrix(rnorm(30), ncol = 3))
#'
#' @export
draw_thompson <- function(model,
                          start,
                          end,
                          xs = NULL) {
  # Input check
  if (!is.list(model)) stop("model must be a list.")
  if (!is.numeric(start) || (start != as.integer(start)) || start <= 0 || is.na(start)) stop("start must be a positive integer without NA values.")
  if (!is.numeric(end) || (end != as.integer(end)) || end < start || is.na(end)) stop("end must be an integer without NA values, and should be greater than or equal to start.")
  if (!is.null(xs)) {
    if (!is.matrix(xs) || any(is.na(xs))) stop("xs must be a matrix without NA values.")
    if (is.null(model$p) || (ncol(xs) != model$p)) stop("The number of columns in xs must be equal to model$p.")
  }
  if (is.null(model$K)) stop("model should have a K field indicating the number of arms.")
  if (!is.logical(model$is_contextual) || is.na(model$is_contextual)) stop("model$is_contextual must be a logical value without NA values.")
  if (is.null(model$num_mc) || !is.numeric(model$num_mc) || (model$num_mc != as.integer(model$num_mc)) || model$num_mc <= 0 || is.na(model$num_mc)) stop("model$num_mc must be a positive integer without NA values.")

  floor <- min(model$floor_start / (model$floor_decay * start), 1 / model$K)

  if (!is.null(xs)) {
    # Draws arms with a LinTS agent for the observed covariates.
    A <- dim(xs)[1]
    p <- dim(xs)[2]
    ps <- array(NA, dim = c(A, model$K))

    xt <- cbind(rep(1, A), xs)
    coeff <- array(NA, dim = c(model$K, model$num_mc, p + 1))
    for (w in 1:model$K) {
      coeff[w, , ] <- MASS::mvrnorm(model$num_mc, model$mu[w, ], model$V[w, , ]) # random.multivariate_normal from different contexts
    }
    draws <- apply(coeff, c(1, 2), function(x) { xt %*% x }) # double-check this line

    for (s in 1:nrow(ps)) { # TODO and double check that draws is doing the right thing here
      ps[s, ] <- table(factor(apply(draws[s, , ], 2, which.max), levels = 1:model$K)) / model$num_mc
      ps[s, ] <- impose_floor(ps[s, ], floor)
    }
    w <- sapply(1:(end - start + 1), function(t) sample(1:model$K, size = 1,
                                                        prob = ps[start + t - 1, ]))

  } else {
    # Draws arms with a non-contextual TS agent.
    if (all(unique(unlist(model$y)) %in% c(0, 1))) { # bernoulli sampling
      successes <- unlist(lapply(model$y, function(x) sum(x)))
      failures <- unlist(lapply(model$y, function(x) length(x) - sum(x)))
      draws <- replicate(model$num_mc, stats::rbeta(model$K, successes + 1, failures + 1)) # + 1 is prior
    } else { # normal approximation sampling
      muhats <- unlist(lapply(model$y, mean))
      sigmahats <- unlist(lapply(model$y, function(x) stats::sd(x) / sqrt(length(x))))
      draws <- replicate(model$num_mc, stats::rnorm(model$K, mean = muhats, sd = sigmahats))
    }
    argmax <- apply(draws, 2, which.max)
    ts_probs <- unname(table(factor(argmax, levels = 1:model$K)) / model$num_mc)
    ps <- impose_floor(ts_probs, floor)
    w <- sample(1:model$K, size = end - start + 1, prob = ps, replace = TRUE)
  }

  return(list(w = w, ps = ps))
}

#' Run an experiment using Thompson Sampling.
#'
#' Runs a LinTS or non-contextual TS bandit experiment, given potential outcomes and covariates.
#'
#' @param ys Matrix. Potential outcomes of shape \code{[A, K]}, where \code{A} is the number of observations and \code{K} is the number of arms. Must not contain NA values.
#' @param floor_start Numeric. Starting value of the floor. Must be a positive number.
#' @param floor_decay Numeric. Decay rate of the floor. Must be a positive number between 0 and 1.
#' @param batch_sizes Integer vector. Size of each batch. Must be positive integers.
#' @param xs Optional matrix. Covariates of shape \code{[A, p]}, where \code{p} is the number of features, if the LinTSModel is contextual. Default is \code{NULL}. Must not contain NA values.
#' @param balanced Optional logical. Indicates whether to balance the batches. Default is \code{NULL}.
#'
#' @return A list containing the pulled arms (\code{ws}), observed rewards (\code{yobs}), assignment probabilities (\code{probs}), and the fitted bandit model (\code{fitted_bandit_model}).
#'
#' @examples
#' set.seed(123)
#' A <- 1000
#' K <- 4
#' xs <- matrix(runif(A * K), nrow = A, ncol = K)
#' ys <- matrix(rbinom(A * K, 1, 0.5), nrow = A, ncol = K)
#' batch_sizes <- c(250, 250, 250, 250)
#' results <- run_experiment(ys = ys,
#'                           floor_start = 5,
#'                           floor_decay = 0.9,
#'                           batch_sizes = batch_sizes,
#'                           xs = xs)
#'
#' @export
run_experiment <- function(ys,
                           floor_start,
                           floor_decay,
                           batch_sizes,
                           xs = NULL,
                           balanced = NULL) {
  # Input check
  if (!is.matrix(ys) || ncol(ys) < 2 || any(is.na(ys))) stop("ys should be a matrix with at least two columns and should not contain NA values.")
  A <- dim(ys)[1] # A: the number of observations
  K <- dim(ys)[2] # K: the number of arms

  if (!is.numeric(floor_start) || floor_start <= 0 || is.na(floor_start)) stop("floor_start should be a positive numeric value without NA values.")
  if (!is.numeric(floor_decay) || floor_decay <= 0 || floor_decay > 1 || is.na(floor_decay)) stop("floor_decay should be a positive numeric value between 0 and 1 without NA values.")
  if (!is.numeric(batch_sizes) || any(batch_sizes <= 0) || any(floor(batch_sizes) != batch_sizes) || any(is.na(batch_sizes))) stop("batch_sizes should be a vector of positive integers without NA values.")
  if (sum(batch_sizes) != A) stop("The total of batch_sizes should equal the number of observations in ys.")
  if (!is.null(xs)) {
    if (!is.matrix(xs) || dim(xs)[1] != A || any(is.na(xs))) stop("xs should be NULL or a matrix with the same number of rows as ys and should not contain NA values.")
  }
  if (!is.null(balanced) && (!is.logical(balanced) || is.na(balanced))) stop("balanced should be NULL or a logical value without NA values.")

  .check_first_batch(batch_sizes, ys)

  ws <- numeric(A) # the index of the selected arm. The ws array is a 1-dimensional array.
  yobs <- numeric(A)

  probs <- array(0, dim = c(A, A, K))
  Probs_t <- matrix(0, A, K)
  p <- dim(xs)[2]

  bandit_model <- LinTSModel(p = p,
                             K = K,
                             floor_start = floor_start,
                             floor_decay = floor_decay,
                             is_contextual = !is.null(xs))

  # uniform sampling at the first batch
  batch_size_cumsum <- cumsum(batch_sizes)
  ws[1:batch_size_cumsum[1]] <- sample( # complete RA in first batch
    c(rep(1:K, batch_size_cumsum[1] %/% K),
      sample(1:K, batch_size_cumsum[1] %% K)),
    batch_size_cumsum[1], replace = FALSE)
  yobs[1:batch_size_cumsum[1]] <- ys[cbind(1:batch_size_cumsum[1], ws[1:batch_size_cumsum[1]])]
  probs[1:batch_size_cumsum[1], , ] <- array(1 / K, dim = c(batch_size_cumsum[1], A, K))

  if (!is.null(xs)) { # contextual case
    bandit_model <- update_thompson(ws = ws[1:batch_size_cumsum[1]],
                                    yobs = yobs[1:batch_size_cumsum[1]],
                                    model = bandit_model,
                                    xs = xs[1:batch_size_cumsum[1], ],
                                    ps = matrix(1 / K, nrow = A, ncol = K)[1:batch_size_cumsum[1], ],
                                    balanced = balanced)
  } else { # non-contextual case
    bandit_model <- update_thompson(ws = ws[1:batch_size_cumsum[1]],
                                    yobs = yobs[1:batch_size_cumsum[1]],
                                    model = bandit_model)
  }

  # adaptive sampling at the subsequent batches
  for (idx in 1:(length(batch_sizes) - 1)) {
    ff <- batch_size_cumsum[idx] + 1
    l <- batch_size_cumsum[idx + 1]
    draw <- draw_thompson(model = bandit_model, start = ff, end = l, xs = xs)
    w <- draw$w
    ps <- draw$ps
    yobs[ff:l] <- ys[cbind(ff:l, w)]
    ws[ff:l] <- w
    probs[ff:l, , ] <- aperm(
      array(matrix(ps, nrow = A, ncol = K, byrow = is.null(xs)),
            dim = dim(probs[ff:l, , ])[c(2, 3, 1)]),
      c(3, 1, 2))

    if (!is.null(xs)) { # contextual case
      bandit_model <- update_thompson(ws = ws[ff:l],
                                      yobs = yobs[ff:l],
                                      model = bandit_model,
                                      xs = xs[ff:l, ],
                                      ps = ps[ff:l, ],
                                      balanced = balanced)
    } else { # non-contextual case
      bandit_model <- update_thompson(ws = ws[ff:l],
                                      yobs = yobs[ff:l],
                                      model = bandit_model)
    }
  }
  # probs are assignment probabilities e_t(X_s, w) of shape [A, A, K]
  if (is.null(xs)) { bandit_model <- NULL }

  data <- list(yobs = yobs, ws = ws, xs = xs, ys = ys, probs = probs, fitted_bandit_model = bandit_model)

  return(data)
}

#' Impose probability floor.
#'
#' Imposes a floor on the given array a, ensuring that its elements are greater than or equal to amin.
#'
#' @param a Numeric vector. Must not contain NA values.
#' @param amin Numeric. Minimum allowed value. Must be between 0 and 1.
#'
#' @return A numeric vector with the same length as \code{a}, with the floor imposed on its elements.
#'
#' @examples
#' a <- c(0.25, 0.25, 0.25, 0.25)
#' imposed_a <- impose_floor(a = a, amin = 0.1)
#'
#' @export
impose_floor <- function(a,
                         amin) {
  # Input check
  if (!is.numeric(a) || length(a) == 0 || any(is.na(a))) stop("a should be a non-empty numeric vector without NA values.")
  if (!is.numeric(amin) || length(amin) != 1 || is.na(amin)) stop("amin should be a single numeric value without NA values.")
  if (amin < 0 || amin > 1) stop("amin should be between 0 and 1.")
  if (all(a < amin)) stop("All elements of a are below amin; this could lead to incorrect results. Please ensure that at least one element of a is equal to or greater than amin.")

  new <- pmax(a, amin)
  total_slack <- sum(new) - 1
  individual_slack <- new - amin
  c <- total_slack / sum(individual_slack)
  new <- new - c * individual_slack
  return(new)
}

#' Generate classification data.
#'
#' Generates covariates and potential outcomes for a classification dataset.
#'
#' @param xs Optional matrix. Covariates of shape \code{[A, p]}, where \code{A} is the number of observations and \code{p} is the number of features. Default is \code{NULL}. Must not contain NA values.
#' @param y Optional vector. Labels of length \code{A}. Default is \code{NULL}. Must not contain NA values.
#' @param noise_std Numeric. Standard deviation of the noise added to the potential outcomes. Default is \code{1.0}. Must be a non-negative number.
#' @param signal_strength Numeric. Strength of the signal in the potential outcomes. Default is \code{1.0}.
#'
#' @return A list containing the generated data (\code{xs}, \code{ys}, \code{muxs}, \code{A}, \code{p}, \code{K}) and the true class probabilities (\code{mus}).
#'
#' @examples
#' data <- generate_bandit_data(xs = iris[,1:4], y = iris[,5], noise_std = 0.1,
#'                              signal_strength = 1.0)
#'
#' @export
generate_bandit_data <- function(xs = NULL,
                                 y = NULL,
                                 noise_std = 1.0,
                                 signal_strength = 1.0) {
  # Input checks
  if (!is.numeric(noise_std) || length(noise_std) != 1 || noise_std < 0 || is.na(noise_std)) stop("noise_std should be a single non-negative numeric value without NA values.")
  if (!is.numeric(signal_strength) || length(signal_strength) != 1 || is.na(signal_strength)) stop("signal_strength should be a single numeric value without NA values.")
  if (!is.null(xs) && !is.null(y)) {
    if (nrow(xs) != length(y)) stop("Number of rows in xs must be equal to the length of y.")
    if (any(is.na(xs))) stop("xs should not contain NA values.")
    if (any(is.na(y))) stop("y should not contain NA values.")
  }

  # Generate covariates and potential outcomes from a classification dataset.

  if (is.null(xs)) {
    xs <- matrix(stats::rnorm(100 * 3), nrow = 100, ncol = 3)
  }
  if (is.null(y)) {
    y <- sample(1:3, size = 100, replace = TRUE)
  }
  A <- nrow(xs)
  A <- min(A, 20000)
  .check_A(A)
  xs <- xs[1:A, ]
  y <- y[1:A]
  K <- length(unique(y))
  muxs <- matrix(0, nrow = A, ncol = K)
  for (k in 1:K) {
    muxs[, k] <- as.integer(y == k) * signal_strength
  }
  ys <- muxs + stats::rnorm(n = A * K, mean = 0, sd = noise_std)
  mus <- table(y) / A
  data <- list(xs = xs, ys = ys, muxs = muxs, A = A, p = ncol(xs), K = K)
  return(list(data = data, mus = mus))
}

#' Generate simple tree data.
#'
#' Generates covariates and potential outcomes of a synthetic dataset for a simple tree model.
#'
#' @param A Integer. Number of observations in the dataset. Must be a positive integer.
#' @param K Integer. Number of arms. Must be a positive integer.
#' @param p Integer. Number of covariates. Must be a positive integer.
#' @param noise_std Numeric. Standard deviation of the noise added to the potential outcomes. Must be a non-negative number.
#' @param split Numeric. Split point for creating treatment groups based on the covariates.
#' @param signal_strength Numeric. Strength of the signal in the potential outcomes.
#' @param seed Optional integer. Seed value for reproducibility. Default is \code{NULL}.
#' @param noise_form Character. Distribution of the noise added to the potential outcomes. Can be either "normal" or "uniform".
#'
#' @return A list containing the generated data (\code{xs}, \code{ys}, \code{muxs}) and the true potential outcome means (\code{mus}).
#'
#' @examples
#' set.seed(123)
#' A <- 1000
#' K <- 4     # Number of treatment arms
#' p <- 10    # Number of covariates
#' synthetic_data <- simple_tree_data(A = A,
#'                                    K = K,
#'                                    p = p,
#'                                    noise_std = 1.0,
#'                                    split = 1.676,
#'                                    signal_strength = 1.0,
#'                                    seed = 123,
#'                                    noise_form = 'normal')
#'
#' @export
simple_tree_data <- function(A, K = 5, p = 10, noise_std = 1.0, split = 1.676,
                             signal_strength = 1.0, seed = NULL, noise_form = 'normal') {
  # Generate covariates and potential outcomes of a synthetic dataset.

  # Input check
  if (!is.numeric(noise_std) || noise_std < 0) stop("noise_std should be a non-negative numeric value.")
  if (!is.numeric(split)) stop("split should be a numeric value.")
  if (!is.numeric(signal_strength)) stop("signal_strength should be a numeric value.")
  if (!is.character(noise_form) || !noise_form %in% c('normal', 'uniform')) stop("noise_form should be either 'normal' or 'uniform'.")
  if (is.na(noise_std) || is.na(split) || is.na(signal_strength) || (!is.null(seed) && is.na(seed)) || is.na(noise_form)) stop("Inputs should not contain NA values.")

  stopifnot(p >= 2) # to check the input parameters satisfy certain conditions
  stopifnot(K >= 4)
  stopifnot(split >= 0)

  set.seed(seed)
  # Generate experimental data
  xs <- matrix(stats::rnorm(A * p), ncol = p)

  r0 <- (xs[, 1] < split) & (xs[, 2] < split)
  r1 <- (xs[, 1] < split) & (xs[, 2] > split)
  r2 <- (xs[, 1] > split) & (xs[, 2] < split)
  r3 <- (xs[, 1] > split) & (xs[, 2] > split)

  muxs <- matrix(0, nrow = A, ncol = K)
  muxs[r0, 1] <- 1
  muxs[r1, 2] <- 1
  muxs[r2, 3] <- 1
  muxs[r3, 4] <- 1
  if (noise_form == 'normal') {
    ys <- muxs + matrix(stats::rnorm(A * K, mean = 0, sd = noise_std), ncol = K)
  } else {
    ys <- muxs + matrix(stats::runif(A * K, min = -noise_std, max = noise_std), ncol = K)
  }

  mvn <- mvtnorm::pmvnorm(upper = c(split, split), mean = rep(0, 2), corr = diag(2))
  mus <- rep(0, K)
  mus[1] <- mvn
  mus[2] <- mvtnorm::pmvnorm(lower = c(-Inf, split), upper = c(split, Inf), mean = rep(0, 2), corr = diag(2))
  mus[3] <- mvtnorm::pmvnorm(lower = c(split, -Inf), upper = c(Inf, split), mean = rep(0, 2), corr = diag(2))
  mus[4] <- mvtnorm::pmvnorm(lower = c(-Inf, -Inf), upper = c(-split, -split), mean = rep(0, 2), corr = diag(2))
  mus <- mus * signal_strength

  data <- list(xs = xs, ys = ys, muxs = muxs)

  return(list(data = data, mus = mus))
}

#' Calculate balancing weight scores.
#'
#' Calculates the inverse probability score of pulling arms, given the actions taken and the true probabilities of each arm being chosen.
#'
#' @param ws Integer vector. Indicates which arm was chosen for observations at each time \code{t}. Length \code{A}. Must not contain NA values.
#' @param probs Numeric matrix or array. True probabilities of each arm being chosen at each time step. Shape \code{[A, K]} or \code{[A, A, K]}. Must not contain NA values.
#'
#' @return A matrix or array containing the inverse probability score of pulling arms.
#'
#' @examples
#' set.seed(123)
#' A <- 5
#' K <- 3
#' ws <- sample(1:K, A, replace = TRUE)
#' probs <- matrix(runif(A * K), nrow = A, ncol = K)
#' balwts <- calculate_balwts(ws, probs)
#'
#' @export
calculate_balwts <- function(ws, probs) {

  # Input check
  if (!is.numeric(ws) || is.null(ws) || any(is.na(ws))) stop("ws should be a non-null numeric vector without NAs.")
  if (!is.numeric(probs) || is.null(probs) || any(is.na(probs))) stop("probs should be a non-null numeric matrix or array without NAs.")
  if (length(ws) != dim(probs)[1]) stop("The length of ws must match the first dimension of probs.")
  if (any(probs <= 0) || any(probs > 1)) stop("Values in probs must be between 0 (exclusive) and 1 (inclusive).")

  A <- length(ws)
  .check_A(A)
  if (length(dim(probs)) == 2) {
    K <- dim(probs)[2]
    balwts <- matrix(0, nrow = A, ncol = K)
    for (a in 1:A) {
      balwts[a, ws[a]] <- 1 / probs[a, ws[a]]
    }
  } else {
    K <- dim(probs)[3]
    balwts <- array(0, dim = c(A, K))
    for (a in 1:A) {
      balwts[a, ] <- 1 / probs[a, a, ]
    }
  }
  return(balwts)
}

#' Plot cumulative assignment for bandit experiment.
#'
#' Generates a plot of the cumulative assignment.
#'
#' @param results List. Results of the experiment, including the actions taken (\code{ws}) and the true probabilities of each arm being chosen (\code{probs}).
#' @param batch_sizes Integer vector. Batch sizes used in the experiment. Must be positive integers.
#'
#' @return A plot of the cumulative assignment.
#'
#' @examples
#' set.seed(123)
#' A <- 1000
#' K <- 4
#' xs <- matrix(runif(A * K), nrow = A, ncol = K)
#' ys <- matrix(rbinom(A * K, 1, 0.5), nrow = A, ncol = K)
#' batch_sizes <- c(250, 250, 250, 250)
#' results <- run_experiment(ys = ys,
#'                           floor_start = 5,
#'                           floor_decay = 0.9,
#'                           batch_sizes = batch_sizes,
#'                           xs = xs)
#' plot_cumulative_assignment(results, batch_sizes)
#'
#' @export
plot_cumulative_assignment <- function(results,
                                       batch_sizes) {
  # Generates a plot of the cumulative assignment graph for every arm and every batch size.
  ws <- results$ws
  A <- length(ws)

  # Input check
  if (!is.list(results)) stop("results should be a list.")
  if (!all(c("ws", "probs") %in% names(results))) stop("results should contain ws and probs.")
  if (!is.numeric(batch_sizes) || any(batch_sizes <= 0) || any(batch_sizes != round(batch_sizes)))
    stop("batch_sizes should be a vector of positive integers.")

  # access dataset components
  .check_A(A)
  K <- dim(results$probs)[length(dim(results$probs))]
  batch_size_cumsum <- cumsum(batch_sizes)

  dat <- matrix(0, nrow = A, ncol = K)
  dat[cbind(1:A, ws)] <- 1
  dat <- apply(dat, 2, cumsum)
  graphics::matplot(dat, type = c("l"), col = 1:K,
                    xlab = "Observations",
                    ylab = "Cumulative assignment",
                    main = "Overall assignment")
  graphics::abline(v = batch_size_cumsum, col = "#00ccff")
  graphics::legend("topleft", legend = 1:K, col = 1:K, lty = 1:K)
}

# Estimating the expected rewards of different arms using ridge regression

#' Ridge Regression Initialization for Arm Expected Rewards
#'
#' Initializes matrices needed for ridge regression to estimate the expected rewards of different arms.
#'
#' @param p Integer. Number of covariates. Must be a positive integer.
#' @param K Integer. Number of arms. Must be a positive integer.
#'
#' @return A list containing initialized matrices \code{R_A}, \code{R_Ainv}, \code{b}, and \code{theta} for each arm.
#'
#' @examples
#' p <- 3
#' K <- 5
#' init <- ridge_init(p, K)
#'
#' @export
ridge_init <- function(p, K) {
  # Initializes matrices needed for ridge regression.

  # Input check
  if (!is.numeric(p) || length(p) != 1 || p <= 0) stop("p should be a positive numeric value.")
  if (!is.numeric(K) || length(K) != 1 || K <= 0) stop("K should be a positive numeric value.")

  R_A <- vector("list", K)
  R_Ainv <- vector("list", K)
  for (k in 1:K) {
    R_A[[k]] <- diag(p + 1)
    R_Ainv[[k]] <- diag(p + 1)
  }
  b <- matrix(0, nrow = K, ncol = p + 1)
  theta <- matrix(0, nrow = K, ncol = p + 1)
  return(list(R_A = R_A, R_Ainv = R_Ainv, b = b, theta = theta))
}

#' Updates ridge regression matrices.
#'
#' Given previous matrices and a new observation, updates the matrices for ridge regression.
#'
#' @param R_A Matrix. Current matrix \code{R_A} for an arm. Must not contain NA values.
#' @param b Numeric vector. Current vector \code{b} for an arm.
#' @param xs Matrix. Covariates of shape \code{[A, p]}, where \code{A} is the number of observations and \code{p} is the number of features. Must not contain NA values.
#' @param t Integer. Current time or instance.
#' @param yobs Numeric vector. Observed outcomes, length \code{A}. Must not contain NA values.
#' @param alpha Numeric. Ridge regression regularization parameter. Default is 1.
#'
#' @return A list containing updated matrices \code{R_A}, \code{R_Ainv}, \code{b}, and \code{theta}.
#'
#' @examples
#' set.seed(123)
#' p <- 3
#' K <- 5
#' init <- ridge_init(p, K)
#' R_A <- init$R_A[[1]]
#' b <- init$b[1, ]
#' xs <- matrix(runif(10 * p), nrow = 10, ncol = p)
#' yobs <- runif(10)
#' t <- 1
#' alpha <- 1
#' updated <- ridge_update(R_A, b, xs, t, yobs[t], alpha)
#'
#' @export
ridge_update <- function(R_A, b, xs, t, yobs, alpha) {
  # Given previous matrices and a new observation, it updates the matrices for ridge regression.

  xt <- xs[t, ]
  xt1 <- c(1, xt)
  R_A <- R_A + xt1 %*% t(xt1) + alpha * diag(length(xt1))
  b <- b + yobs * xt1
  R_Ainv <- solve(R_A)
  theta <- R_Ainv %*% b
  return(list(R_A = R_A, R_Ainv = R_Ainv, b = b, theta = theta))
}

#' Leave-future-out ridge-based estimates for arm expected rewards.
#'
#' Computes leave-future-out ridge-basedn estimates of arm expected rewards based on provided data.
#'
#' @param xs Matrix. Covariates of shape \code{[A, p]}, where \code{A} is the number of observations and \code{p} is the number of features. Must not contain NA values.
#' @param ws Integer vector. Indicates which arm was chosen for observations at each time \code{t}. Length \code{A}. Must not contain NA values.
#' @param yobs Numeric vector. Observed outcomes, length \code{A}. Must not contain NA values.
#' @param K Integer. Number of arms. Must be a positive integer.
#' @param batch_sizes Integer vector. Sizes of batches in which data is processed. Must be positive integers.
#' @param alpha Numeric. Ridge regression regularization parameter. Default is 1.
#'
#' @return A 3D array containing the expected reward estimates for each arm and each time \code{t}, of shape \code{[A, A, K]}.
#'
#' @examples
#' set.seed(123)
#' p <- 3
#' K <- 5
#' A <- 100
#' xs <- matrix(runif(A * p), nrow = A, ncol = p)
#' ws <- sample(1:K, A, replace = TRUE)
#' yobs <- runif(A)
#' batch_sizes <- c(25, 25, 25, 25)
#' muhat <- ridge_muhat_lfo_pai(xs, ws, yobs, K, batch_sizes)
#' print(muhat)
#'
#' @export
ridge_muhat_lfo_pai <- function(xs, ws, yobs, K, batch_sizes, alpha = 1) {
  # Return plug-in estimates of arm expected reward.

  #   muhat: The expected reward estimates for each arm and each time t, of shape [A, A, K].
  if (!is.matrix(xs) || ncol(xs) <= 0) stop("xs should be a matrix with more than 0 columns.")
  if (length(ws) != nrow(xs) || length(yobs) != nrow(xs)) stop("Lengths of ws and yobs should match the number of rows in xs.")

  A <- nrow(xs)
  p <- ncol(xs)
  result <- ridge_init(p, K)
  R_A <- result$R_A
  R_Ainv <- result$R_Ainv
  b <- result$b
  theta <- result$theta
  muhat <- array(0, dim = c(A, A, K))

  batch_cumsum <- c(0, cumsum(batch_sizes))

  for (idx in 1:(length(batch_cumsum) - 1)) {
    l <- batch_cumsum[idx] + 1
    r <- batch_cumsum[idx + 1]
    xt1 <- matrix(0, nrow = p + 1, ncol = A)
    xt1[1, ] <- 1
    xt1[2:(p + 1), ] <- t(xs)
    for (w in 1:K) {
      muhat[l:r, , w] <- t(theta[w, ]) %*% xt1
    }
    for (t in l:r) {
      w <- ws[t]
      result <- ridge_update(R_A[[w]], b[w, ], xs, t, yobs[t], alpha)
      R_A[[w]] <- result$R_A
      R_Ainv[[w]] <- result$R_Ainv
      b[w, ] <- result$b
      theta[w, ] <- result$theta
    }
  }
  return(muhat)
}
