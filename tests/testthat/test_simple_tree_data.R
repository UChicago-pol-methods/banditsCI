library(testthat)

test_that("simple_tree_data generates the expected data structure and dimensions", {
  # Set the seed for reproducibility
  set.seed(123)

  # Call the simple_tree_data function
  result <- simple_tree_data(A = 100, K = 5, p = 10, noise_std = 1.0, split = 1.676, signal_strength = 1.0, seed = 456, noise_form = 'normal')

  # Perform assertions to check the output structure and dimensions
  expect_true(is.list(result))
  expect_true("data" %in% names(result))
  expect_true("mus" %in% names(result))

  expect_true(is.list(result$data))
  expect_true("xs" %in% names(result$data))
  expect_true("ys" %in% names(result$data))
  expect_true("muxs" %in% names(result$data))

  expect_equal(dim(result$data$xs), c(100, 10))
  expect_equal(dim(result$data$ys), c(100, 5))
  expect_equal(dim(result$data$muxs), c(100, 5))

  expect_true(is.numeric(result$mus))
  expect_equal(length(result$mus), 5)
})

test_that("simple_tree_data throws an error for invalid input parameters", {
  expect_error(simple_tree_data(A = 100, K = 3, p = 10, noise_std = 1.0, split = 1.676, signal_strength = 1.0, seed = 456, noise_form = 'normal'))
  expect_error(simple_tree_data(A = 100, K = 5, p = 1, noise_std = 1.0, split = 1.676, signal_strength = 1.0, seed = 456, noise_form = 'normal'))
  expect_error(simple_tree_data(A = 100, K = 5, p = 10, noise_std = 1.0, split = -1, signal_strength = 1.0, seed = 456, noise_form = 'normal'))
})
