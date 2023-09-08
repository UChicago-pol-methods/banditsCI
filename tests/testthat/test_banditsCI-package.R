library(testthat)

test_that("impose_floor correctly imposes a floor on the given vector", {
  a <- c(0.25, 0.25, 0.25, 0.25)
  amin <- 0.1

  imposed_a <- impose_floor(a = a, amin = amin)

  # Assertions for the structure and values of the imposed_a

  expect_true(is.numeric(imposed_a), "The imposed_a should be a numeric vector")
  expect_equal(length(imposed_a), length(a), info = "The length of imposed_a should be the same as the input vector")

  expect_true(all(imposed_a >= amin), "All elements of imposed_a should be greater than or equal to amin")
  expect_true(abs(sum(imposed_a) - 1) < 1e-10, "The sum of elements in imposed_a should be approximately equal to 1")

  # Additional assertions specific to the values of imposed_a

  expect_true(all(imposed_a <= a), "All elements of imposed_a should be less than or equal to the corresponding elements in a")
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
  expect_equal(dim(balwts)[1], A, info = "The number of rows in the inverse probability scores should be equal to 'A'")
  expect_equal(dim(balwts)[2], K, info = "The number of columns in the inverse probability scores should be equal to 'K'")

  if (length(dim(probs)) == 2) {
    for (a in 1:A) {
      if (ws[a] > K) {
        expect_equal(balwts[a, ws[a]], 0, info = "The inverse probability score should be 0 if the action is out of range")
      } else {
        expect_equal(balwts[a, ws[a]], 1/probs[a, ws[a]], info = "The inverse probability score should be the reciprocal of the true probability")
      }
    }
  } else {
    for (a in 1:A) {
      expect_equal(balwts[a, ], 1/probs[a, a, ], info = "The inverse probability scores should be the reciprocal of the true probabilities")
    }
  }
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

test_that("ifelse_clip works", {
  # Test basic functionality.
  lamb <- c(1, 2, 3, 4, 5)
  expect_equal(ifelse_clip(lamb, 2, 4), c(2, 2, 3, 4, 4))
})
