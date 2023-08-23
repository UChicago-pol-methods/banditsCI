library(testthat)

test_that("LinTSModel creates the model with the correct parameters", {
  model <- LinTSModel(K = 5, p = 3, floor_start = 1, floor_decay = 0.9, num_mc = 100, is_contextual = TRUE)

  expect_equal(model$K, 5, "The number of arms should be 5")
  expect_equal(model$p, 3, "The dimension of the contextual vector should be 3")
  expect_equal(model$floor_start, 1, "The initial floor value should be 1")
  expect_equal(model$floor_decay, 0.9, "The decay rate of the floor value should be 0.9")
  expect_equal(model$num_mc, 100, "The number of Monte Carlo simulations should be 100")
  expect_true(model$is_contextual, "The problem should be contextual")

  # Additional assertions for the structure of the model if it is_contextual is TRUE

  if (model$is_contextual) {
    expect_equal(dim(model$mu), c(5, 4), "The shape of mu should be (5, 4)")
    expect_equal(dim(model$V), c(5, 4, 4), "The shape of V should be (5, 4, 4)")
    expect_equal(dim(model$X), c(5, 0, 3), "The shape of X should be (5, 0, 3)")
  }
})

test_that("update_thompson updates the model parameters correctly", {
  library(glmnet)  # Load glmnet package

  model <- LinTSModel(K = 5, p = 3, floor_start = 1, floor_decay = 0.9, num_mc = 100, is_contextual = TRUE)
  xs <- matrix(rnorm(9), ncol = 3)
  ws <- c(3, 2, 1)
  yobs <- c(1, 0, 1)
  ps <- matrix(runif(3), nrow = 3, ncol = 5)
  balanced <- FALSE

  updated_model <- update_thompson(ws, yobs, model, xs, ps, balanced)

  # Assertions for the updated parameters

  expect_equal(dim(updated_model$X[[1]]), c(1, 3), "The shape of X for arm 1 should be (1, 3)")
  expect_equal(dim(updated_model$X[[2]]), c(1, 3), "The shape of X for arm 2 should be (1, 3)")
  expect_equal(dim(updated_model$X[[3]]), c(1, 3), "The shape of X for arm 3 should be (1, 3)")
  expect_equal(dim(updated_model$y[[1]]), c(1, 1), "The shape of y for arm 1 should be (1, 1)")
  expect_equal(dim(updated_model$y[[2]]), c(1, 1), "The shape of y for arm 2 should be (1, 1)")
  expect_equal(dim(updated_model$y[[3]]), c(1, 1), "The shape of y for arm 3 should be (1, 1)")
  expect_equal(dim(updated_model$ps[[1]]), c(1, 5), "The shape of ps for arm 1 should be (1, 5)")
  expect_equal(dim(updated_model$ps[[2]]), c(1, 5), "The shape of ps for arm 2 should be (1, 5)")
  expect_equal(dim(updated_model$ps[[3]]), c(1, 5), "The shape of ps for arm 3 should be (1, 5)")

  # Additional assertions for the structure of the updated model if it is_contextual is TRUE

  if (updated_model$is_contextual) {
    expect_equal(dim(updated_model$mu), c(5, 4), "The shape of mu should be (5, 4)")
    expect_equal(dim(updated_model$V), c(5, 4, 4), "The shape of V should be (5, 4, 4)")
  }
})

test_that("draw_thompson correctly draws arms and probabilities", {
  model <- LinTSModel(K = 5, p = 3, floor_start = 1, floor_decay = 0.9, num_mc = 100, is_contextual = TRUE)
  start <- 1
  end <- 10
  xs <- matrix(rnorm(30), ncol = 3)

  result <- draw_thompson(model, start, end, xs)

  # Assertions for the structure and values of the result

  expect_true(is.list(result), "The result should be a list")
  expect_true("w" %in% names(result), "The result should contain 'w'")
  expect_true("ps" %in% names(result), "The result should contain 'ps'")

  expect_equal(length(result$w), end - start + 1, "The length of 'w' should be (end - start + 1)")
  expect_equal(dim(result$ps), c(end - start + 1, model$K), "The shape of 'ps' should be (end - start + 1, K)")

  # Additional assertions specific to the contextual case

  if (!is.null(xs)) {
    expect_equal(dim(xs), c(end - start + 1, model$p), "The shape of 'xs' should be (end - start + 1, p)")
  }
})

test_that("run_experiment correctly runs the bandit experiment", {
  ys <- matrix(rbinom(1000, 1, 0.5), ncol = 5)
  xs <- matrix(rnorm(300), ncol = 3)
  batch_sizes <- c(100, 200, 700)

  result <- run_experiment(ys = ys, floor_start = 1, floor_decay = 0.9, batch_sizes = batch_sizes, xs = xs, balanced = TRUE)

  # Assertions for the structure and values of the result

  expect_true(is.list(result), "The result should be a list")
  expect_true("yobs" %in% names(result), "The result should contain 'yobs'")
  expect_true("ws" %in% names(result), "The result should contain 'ws'")
  expect_true("xs" %in% names(result), "The result should contain 'xs'")
  expect_true("ys" %in% names(result), "The result should contain 'ys'")
  expect_true("probs" %in% names(result), "The result should contain 'probs'")
  expect_true("fitted_bandit_model" %in% names(result), "The result should contain 'fitted_bandit_model'")

  expect_equal(length(result$yobs), sum(batch_sizes), "The length of 'yobs' should be the sum of batch_sizes")
  expect_equal(length(result$ws), sum(batch_sizes), "The length of 'ws' should be the sum of batch_sizes")

  if (!is.null(xs)) {
    expect_equal(dim(result$xs), c(sum(batch_sizes), ncol(xs)), "The shape of 'xs' should be (sum(batch_sizes), p)")
  }

  expect_equal(dim(result$ys), c(dim(ys)[1], dim(ys)[2]), "The shape of 'ys' should be the same as the input ys")
  expect_equal(dim(result$probs), c(sum(batch_sizes), dim(ys)[1], dim(ys)[2]), "The shape of 'probs' should be (sum(batch_sizes), A, K)")

  # Additional assertions specific to the contextual case

  if (!is.null(xs)) {
    expect_true(is.null(result$fitted_bandit_model), "'fitted_bandit_model' should be NULL in the contextual case")
  } else {
    expect_true(!is.null(result$fitted_bandit_model), "'fitted_bandit_model' should not be NULL in the non-contextual case")
  }
})

test_that("impose_floor correctly imposes a floor on the given vector", {
  a <- c(0.25, 0.25, 0.25, 0.25)
  amin <- 0.1

  imposed_a <- impose_floor(a = a, amin = amin)

  # Assertions for the structure and values of the imposed_a

  expect_true(is.numeric(imposed_a), "The imposed_a should be a numeric vector")
  expect_equal(length(imposed_a), length(a), "The length of imposed_a should be the same as the input vector")

  expect_true(all(imposed_a >= amin), "All elements of imposed_a should be greater than or equal to amin")
  expect_true(abs(sum(imposed_a) - 1) < 1e-10, "The sum of elements in imposed_a should be approximately equal to 1")

  # Additional assertions specific to the values of imposed_a

  expect_true(all(imposed_a <= a), "All elements of imposed_a should be less than or equal to the corresponding elements in a")
})

test_that("simple_tree_data correctly generates covariates and potential outcomes", {
  set.seed(123)

  A <- 100
  K <- 5
  p <- 10
  noise_std <- 1.0
  split <- 1.676
  signal_strength <- 1.0
  seed <- NULL
  noise_form <- "normal"

  data <- simple_tree_data(A = A, K = K, p = p, noise_std = noise_std, split = split,
                           signal_strength = signal_strength, seed = seed, noise_form = noise_form)

  # Assertions for the structure and values of the generated data

  expect_true(is.list(data), "The generated data should be a list")
  expect_true("data" %in% names(data), "The generated data should contain 'data'")
  expect_true("mus" %in% names(data), "The generated data should contain 'mus'")

  generated_data <- data$data

  expect_true(is.list(generated_data), "The generated data should be a list")
  expect_true("xs" %in% names(generated_data), "The generated data should contain 'xs'")
  expect_true("ys" %in% names(generated_data), "The generated data should contain 'ys'")
  expect_true("muxs" %in% names(generated_data), "The generated data should contain 'muxs'")
  expect_true("wxs" %in% names(generated_data), "The generated data should contain 'wxs'")

  expect_true(is.matrix(generated_data$xs), "The generated 'xs' should be a matrix")
  expect_true(is.matrix(generated_data$ys), "The generated 'ys' should be a matrix")
  expect_true(is.matrix(generated_data$muxs), "The generated 'muxs' should be a matrix")
  expect_true(is.matrix(generated_data$wxs), "The generated 'wxs' should be a matrix")

  expect_equal(dim(generated_data$xs)[1], A, "The number of rows in 'xs' should be equal to 'A'")
  expect_equal(dim(generated_data$xs)[2], p, "The number of columns in 'xs' should be equal to 'p'")
  expect_equal(dim(generated_data$ys)[1], A, "The number of rows in 'ys' should be equal to 'A'")
  expect_equal(dim(generated_data$ys)[2], K, "The number of columns in 'ys' should be equal to 'K'")
  expect_equal(dim(generated_data$muxs)[1], A, "The number of rows in 'muxs' should be equal to 'A'")
  expect_equal(dim(generated_data$muxs)[2], K, "The number of columns in 'muxs' should be equal to 'K'")
  expect_equal(dim(generated_data$wxs)[1], A, "The number of rows in 'wxs' should be equal to 'A'")
  expect_equal(dim(generated_data$wxs)[2], K, "The number of columns in 'wxs' should be equal to 'K'")

  expect_true(all(generated_data$xs >= -Inf & generated_data$xs <= Inf), "All elements of 'xs' should be within the expected range")
  expect_true(all(generated_data$ys >= -Inf & generated_data$ys <= Inf), "All elements of 'ys' should be within the expected range")
  expect_true(all(generated_data$muxs >= 0), "All elements of 'muxs' should be non-negative")
  expect_true(all(generated_data$wxs %in% c(0, 1)), "All elements of 'wxs' should be either 0 or 1")

  # Assertions for the structure and values of the true class probabilities

  true_class_probs <- data$mus

  expect_true(is.numeric(true_class_probs), "The true class probabilities should be a numeric vector")

  # Additional assertions specific to the values of true_class_probs

  expect_equal(length(true_class_probs), K, "The length of true_class_probs should be equal to 'K'")
  expect_true(all(true_class_probs >= 0), "All class probabilities should be non-negative")
  expect_true(all(true_class_probs <= signal_strength), "All class probabilities should be less than or equal to 'signal_strength'")
})

test_that("calculate_balwts correctly calculates the inverse probability scores", {
  set.seed(123)

  A <- 5
  K <- 3

  # Generate random actions and probabilities
  ws <- sample(1:K, A, replace = TRUE)
  probs <- matrix(runif(A * K), nrow = A, ncol = K)

  # Calculate inverse probability scores
  balwts <- calculate_balwts(ws, probs)

  # Assertions for the structure and values of the inverse probability scores

  expect_true(is.matrix(balwts), "The inverse probability scores should be a matrix")
  expect_equal(dim(balwts)[1], A, "The number of rows in the inverse probability scores should be equal to 'A'")
  expect_equal(dim(balwts)[2], K, "The number of columns in the inverse probability scores should be equal to 'K'")

  if (length(dim(probs)) == 2) {
    for (a in 1:A) {
      if (ws[a] > K) {
        expect_equal(balwts[a, ws[a]], 0, "The inverse probability score should be 0 if the action is out of range")
      } else {
        expect_equal(balwts[a, ws[a]], 1/probs[a, ws[a]], "The inverse probability score should be the reciprocal of the true probability")
      }
    }
  } else {
    for (a in 1:A) {
      expect_equal(balwts[a, ], 1/probs[a, a, ], "The inverse probability scores should be the reciprocal of the true probabilities")
    }
  }
})

test_that("calculate_mu_hat correctly calculates the estimated expected values of the reward", {
  set.seed(123)

  A <- 200
  K <- p <- 3

  # Generate random covariate matrix and outcomes
  xs <- matrix(runif(A * p), nrow = A, ncol = p)
  ys <- matrix(rbinom(A * K, 1, 0.5), nrow = A, ncol = K)

  # Create a mock results object with a fitted bandit model and covariate matrix
  results <- list(
    fitted_bandit_model = list(
      K = K,
      mu = matrix(runif(K * (p + 1)), nrow = K, ncol = p + 1)
    ),
    xs = xs,
    yobs = ys
  )

  # Calculate estimated expected values of the reward
  mu_hat <- calculate_mu_hat(results)

  # Assertions for the structure and values of the estimated expected values

  expect_true(is.matrix(mu_hat), "The estimated expected values should be a matrix")
  expect_equal(dim(mu_hat)[1], A, "The number of rows in the estimated expected values should be equal to 'A'")
  expect_equal(dim(mu_hat)[2], K, "The number of columns in the estimated expected values should be equal to 'K'")

  # Additional assertions specific to the values of mu_hat
  coefhat <- results$fitted_bandit_model$mu
  X <- cbind(1, xs)
  expected_mu_hat <- X %*% t(coefhat)

  expect_equal(mu_hat, expected_mu_hat, "The estimated expected values should match the expected values based on the fitted model")
})

test_that("plot_cumulative_assignment correctly generates the cumulative assignment graph", {
  set.seed(123)

  A <- 5
  K <- 3

  # Generate random actions and true probabilities
  ws <- sample(1:K, A, replace = TRUE)
  probs <- matrix(runif(A * K), nrow = A, ncol = K)

  # Create a mock results object
  results <- list(
    ws = ws,
    probs = probs
  )

  # Define batch sizes
  batch_sizes <- c(10, 20, 30)

  # Call the plot_cumulative_assignment function
  plot_cumulative_assignment(results, batch_sizes)

  # No assertions for the plot output, as we can visually inspect the generated plot

  # Additional assertions to check if the function runs without errors
  expect_no_error(plot_cumulative_assignment(results, batch_sizes))
})

test_that("aw_var computes variance of policy value estimator correctly", {
  scores <- matrix(c(0.5, 0.8, 0.6, 0.3, 0.9, 0.2, 0.5, 0.7, 0.4, 0.8, 0.2, 0.6), ncol = 3)
  policy <- matrix(c(0.2, 0.3, 0.5, 0.6, 0.1, 0.3, 0.4, 0.5, 0.1, 0.2, 0.7, 0.1), ncol = 3)
  estimate <- aw_estimate(scores = scores, policy = policy, evalwts = c(0.5, 1, 0.5, 1.5))
  variance <- aw_var(scores = scores, estimate = estimate, policy = policy, evalwts = c(0.5, 1, 0.5, 1.5))

  # Assertion for the computed variance
  expect_equal(variance, 0.00625, "The computed variance should be 0.00625")
})

test_that("estimate computes estimate and variance of policy evaluation correctly", {
  w <- c(0.5, 1, 0.5, 1.5)
  scores <- matrix(c(0.5, 0.8, 0.6, 0.3, 0.9, 0.2, 0.5, 0.7, 0.4, 0.8, 0.2, 0.6), ncol = 3)
  policy <- matrix(c(0.2, 0.3, 0.5, 0.6, 0.1, 0.3, 0.4, 0.5, 0.1, 0.2, 0.7, 0.1), ncol = 3)
  gammahat <- scores - policy
  result <- estimate(w = w, gammahat = gammahat, policy = policy)

  # Assertions for the computed estimate and variance
  expect_equal(result[["estimate"]], 0.925, "The estimated policy value should be 0.925")
  expect_equal(result[["var"]], 0.00625, "The computed variance should be 0.00625")
})

test_that("calculate_continuous_X_statistics computes estimate and variance of policy evaluation correctly", {
  h <- matrix(c(0.4, 0.3, 0.2, 0.1, 0.2, 0.3, 0.3, 0.2, 0.5, 0.3, 0.2, 0.1, 0.1, 0.2, 0.1, 0.6), ncol = 4)
  scores <- matrix(c(0.5, 0.8, 0.6, 0.3, 0.9, 0.2, 0.5, 0.7, 0.4, 0.8, 0.2, 0.6), ncol = 3)
  policy <- matrix(c(0.2, 0.3, 0.5, 0.6, 0.1, 0.3, 0.4, 0.5, 0.1, 0.2, 0.7, 0.1), ncol = 3)
  gammahat <- scores - policy
  result <- calculate_continuous_X_statistics(h = h, gammahat = gammahat, policy = policy)

  # Assertions for the computed estimate and variance
  expect_equal(result[["estimate"]], 0.542, "The estimated policy value should be 0.542")
  expect_equal(result[["var"]], 0.003721, "The computed variance should be 0.003721")
})

test_that("stick_breaking works", {
  # Test basic functionality.
  Z <- array(runif(10), dim = c(2, 5))
  weights <- stick_breaking(Z)
  expect_equal(sum(weights), 1.0)
  for (a in 1:2) {
    expect_equal(sum(weights[a,]), 1.0)
    for (k in 1:5) {
      expect_lte(weights[a, k], 1.0)
    }
  }
})

test_that("ifelse_clip works", {
  # Test basic functionality.
  lamb <- c(1, 2, 3, 4, 5)
  expect_equal(ifelse_clip(lamb, 2, 4), c(2, 2, 3, 4, 4))
})
