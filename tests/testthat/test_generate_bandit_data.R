library(testthat)

# tests/testthat/test_generate_bandit_data.R

test_that("generate_bandit_data generates the expected data structure", {
  # Call the generate_bandit_data function
  generated_data <- generate_bandit_data(noise_std=0,
                                         signal_strength=1.0)

  # Perform assertions to check the expected structure of the generated data
  expect_true(is.list(generated_data))
  expect_true("data" %in% names(generated_data))
  expect_true("mus" %in% names(generated_data))
  expect_true(is.list(generated_data$data))

  # Check the structure and dimensions of the data
  data <- generated_data$data
  expect_true("xs" %in% names(data))
  expect_true("ys" %in% names(data))
  expect_true("muxs" %in% names(data))
  expect_true("A" %in% names(data))
  expect_true("p" %in% names(data))
  expect_true("K" %in% names(data))

  xs <- data$xs
  ys <- data$ys
  muxs <- data$muxs
  A <- data$A
  p <- data$p
  K <- data$K

  expect_true(is.matrix(xs))
  expect_equal(dim(xs)[1], A)
  expect_equal(dim(xs)[2], p)

  expect_true(is.matrix(ys))
  expect_equal(dim(ys)[1], A)
  expect_equal(dim(ys)[2], K)

  expect_true(is.matrix(muxs))
  expect_equal(dim(muxs)[1], A)
  expect_equal(dim(muxs)[2], K)

  expect_true(is.numeric(ys))

  expect_true(is.numeric(A))
  expect_true(A <= 20000)

  expect_true(is.integer(p))

  expect_true(is.integer(K))

  # Check the structure and values of the mus
  mus <- generated_data$mus
  expect_true(is.table(mus))
  expect_equal(sum(mus), 1)
})

test_that("generate_bandit_data generates data with custom parameters", {
  # Set custom parameters
  noise_std <- 0.5
  signal_strength <- 0.8

  # Call the generate_bandit_data function with custom parameters
  generated_data <- generate_bandit_data(noise_std = noise_std,
                                         signal_strength = signal_strength)

  # Perform assertions to check the expected values
  ys <- generated_data$data$ys
  muxs <- generated_data$data$muxs

  expect_equal(sd(ys-muxs), noise_std, tolerance = .1) #inexact w/ small n
  expect_true(all(muxs <= signal_strength))
})
