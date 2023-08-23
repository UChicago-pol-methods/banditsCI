# tests/testthat/test_aw_scores.R
library(testthat)

test_that("aw_scores returns the expected AIPW scores", {
  # Create sample data for testing
  A <- 200
  K <- 3
  yobs <- rnorm(A)
  ws <- sample(1:K, A, replace = TRUE)
  balwts <- matrix(runif(A * K), nrow = A, ncol = K)
  mu_hat <- matrix(rnorm(A * K), nrow = A, ncol = K)

  # Call the aw_scores function
  scores <- aw_scores(yobs, ws, balwts, K, mu_hat)

  # Define the expand function
  expand <- function(mat, indices, ncol) {
    output <- matrix(0, nrow(mat), ncol)
    for (i in 1:nrow(mat)) {
      output[i, indices[i]] <- mat[i, indices[i]]
    }
    return(output)
  }

  # Perform assertions to check the expected values
  expected_scores <- balwts * yobs
  expected_scores[cbind(1:A, ws)] <- scores[cbind(1:A, ws)]
  expected_scores <- expected_scores + (1 - expand(balwts, ws, K)) * mu_hat
  expect_equal(dim(scores), dim(expected_scores))
  expect_equal(scores, expected_scores)
})

test_that("aw_scores returns the expected IPW scores when mu_hat is NULL", {
  # Create sample data for testing
  A <- 200
  K <- 3
  yobs <- rnorm(A)
  ws <- sample(1:K, A, replace = TRUE)
  balwts <- matrix(runif(A * K), nrow = A, ncol = K)

  # Call the aw_scores function without mu_hat
  scores <- aw_scores(yobs, ws, balwts, K)

  # Perform assertions to check the expected values (IPW scores)
  expected_scores <- balwts * yobs
  expect_equal(dim(scores), dim(expected_scores))
  expect_equal(scores, expected_scores)
})
