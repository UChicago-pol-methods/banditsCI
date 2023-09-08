library(testthat)

# Test case 1: Single-arm estimation with uniform weights
test_that("Single-arm estimation with uniform weights", {
  # Generate test data
  policy1 <- list(matrix(c(0.2, 0.8,
                           0.3, 0.7), nrow = 2))
  gammahat <- matrix(c(0.5, 0.3,
                       0.6, 0.1), nrow = 2)
  contextual_probs <- array(rep(0.5, 8), dim = c(2, 2, 2))

  # Call the function
  result <- output_estimates(policy1 = policy1,
                             gammahat = gammahat,
                             contextual_probs = contextual_probs,
                             uniform = TRUE)

  # Perform assertions on the result
  expect_equal(length(result), 1)
  expect_equal(dim(result[[1]]), c(6, 2))
  # Add more assertions as needed
})

# Test case 2: Two-arm estimation with non-contextual minvar weights
test_that("Two-arm estimation with non-contextual minvar weights", {
  # Generate test data
  policy0 <- matrix(c(0.3, 0.7,
                      0.2, 0.8), nrow = 2)
  policy1 <- list(matrix(c(0.2, 0.8,
                           0.3, 0.7), nrow = 2))
  gammahat <- matrix(c(0.5, 0.3,
                       0.6, 0.1), nrow = 2)
  contextual_probs <- array(rep(0.5, 8), dim = c(2, 2, 2))

  # Call the function
  result <- output_estimates(policy0 = policy0, policy1 = policy1, gammahat = gammahat,
                             contextual_probs = contextual_probs,
                             non_contextual_minvar = TRUE)

  # Perform assertions on the result
  expect_equal(length(result), 1)
  expect_equal(dim(result[[1]]), c(6, 2))
  # Add more assertions as needed
})
