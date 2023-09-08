# tests/testthat/test_calculate_mu_hat.R
library(testthat)

test_that("calculate_mu_hat returns the expected estimated expected values", {
  # Create sample data for testing
  A <- 200
  K <- 3
  xs <- matrix(runif(A * K), nrow = A, ncol = K)
  ys <- matrix(rnorm(A * K), nrow = A, ncol = K)
  ws <- sample(1:K, A, replace = TRUE)
  results <- list(
    fitted_bandit_model = list(
      K = K,
      mu = matrix(rnorm(K * (K + 1)), nrow = K)
    ),
    yobs = ys[cbind(1:A,ws)],
    xs = xs
  )

  # Call the calculate_mu_hat function
  mu_hat <- calculate_mu_hat(results)

  # Perform assertions to check the expected values
  expected_mu_hat <- matrix(NA, nrow = A, ncol = K)
  for (w in 1:K) {
    coefhat <- results$fitted_bandit_model$mu[w, ]
    expected_mu_hat[, w] <- cbind(1, results$xs) %*% coefhat
  }

  expect_equal(dim(mu_hat), dim(expected_mu_hat))
  expect_equal(mu_hat, expected_mu_hat)
})
