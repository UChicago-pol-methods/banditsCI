# tests/testthat/test_aw_var.R

test_that("aw_var calculates the variance of the policy value estimate", {
  # Create sample data for testing
  A <- 100
  K <- 3
  scores <- matrix(rnorm(A * K), nrow = A, ncol = K)
  estimate <- 0.5
  policy <- matrix(runif(A * K), nrow = A, ncol = K)
  evalwts <- runif(A)

  # Call the aw_var function
  variance <- aw_var(scores, estimate, policy, evalwts)

  # Perform assertions to check the expected variance
  expected_variance <- sum((rowSums(policy * scores) - estimate)^2 * evalwts^2) / sum(evalwts)^2
  expect_equal(variance, expected_variance)
})
