library(testthat)

# tests/testthat/test_aw_estimate.R

test_that("aw_estimate returns the expected policy value", {
  # Create sample data for testing
  A <- 200
  K <- 3
  scores <- matrix(rnorm(A * K), nrow = A, ncol = K)
  policy <- matrix(runif(A * K), nrow = A, ncol = K)
  evalwts <- rnorm(A)

  # Call the aw_estimate function
  estimated_value <- aw_estimate(scores, policy, evalwts)

  # Perform assertions to check the expected value
  expected_value <- sum(evalwts * rowSums(scores * policy)) / sum(evalwts)
  expect_equal(estimated_value, expected_value)
})

test_that("aw_estimate returns the expected policy value when evalwts is NULL", {
  # Create sample data for testing
  A <- 200
  K <- 3
  scores <- matrix(rnorm(A * K), nrow = A, ncol = K)
  policy <- matrix(runif(A * K), nrow = A, ncol = K)

  # Call the aw_estimate function without evalwts
  estimated_value <- aw_estimate(scores, policy)

  # Perform assertions to check the expected value
  expected_value <- sum(rowSums(scores * policy)) / (A * K)
  expect_equal(estimated_value, expected_value)
})
