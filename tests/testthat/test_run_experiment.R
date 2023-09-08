library(testthat)

test_that("run_experiment correctly runs a bandit experiment for a non-contextual case", {
  # Generate sample data
  ys <- matrix(runif(20), nrow = 10, ncol = 2)
  floor_start <- 1
  floor_decay <- 0.9
  batch_sizes <- c(5, 5)

  # Call the run_experiment function
  result <- run_experiment(ys = ys,
                           floor_start = floor_start,
                           floor_decay = floor_decay,
                           batch_sizes = batch_sizes)

  # Perform assertions to check if the output data has the correct structure and dimensions
  expect_true(is.list(result))
  expect_true("yobs" %in% names(result))
  expect_true("ws" %in% names(result))
  expect_true("xs" %in% names(result))
  expect_true("ys" %in% names(result))
  expect_true("probs" %in% names(result))
  expect_true("fitted_bandit_model" %in% names(result))

  expect_equal(length(result$yobs), dim(ys)[1])
  expect_equal(length(result$ws), sum(batch_sizes))
  expect_type(result$xs, "NULL")
  expect_equal(dim(result$ys), dim(ys))
  expect_equal(dim(result$probs), c(sum(batch_sizes), dim(ys)[1], dim(ys)[2]))
  expect_type(result$fitted_bandit_model, "NULL")
})

test_that("run_experiment correctly runs a bandit experiment for a contextual case", {
  # Generate sample data
  ys <- matrix(runif(20), nrow = 10, ncol = 2)
  floor_start <- 1
  floor_decay <- 0.9
  batch_sizes <- c(5, 5)
  xs <- matrix(rnorm(20), nrow = 10, ncol = 2)
  balanced <- TRUE

  # Call the run_experiment function
  result <- run_experiment(ys = ys, floor_start = floor_start, floor_decay = floor_decay, batch_sizes = batch_sizes, xs = xs, balanced = balanced)

  # Perform assertions to check if the output data has the correct structure and dimensions
  expect_true(is.list(result))
  expect_true("yobs" %in% names(result))
  expect_true("ws" %in% names(result))
  expect_true("xs" %in% names(result))
  expect_true("ys" %in% names(result))
  expect_true("probs" %in% names(result))
  expect_true("fitted_bandit_model" %in% names(result))

  expect_equal(length(result$yobs), dim(ys)[1])
  expect_equal(length(result$ws), sum(batch_sizes))
  expect_equal(dim(result$xs), dim(xs))
  expect_equal(dim(result$ys), dim(ys))
  expect_equal(dim(result$probs), c(sum(batch_sizes), dim(ys)[1], dim(ys)[2]))
  expect_true(!is.null(result$fitted_bandit_model))
})
