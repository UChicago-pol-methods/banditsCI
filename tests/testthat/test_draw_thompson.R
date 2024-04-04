library(testthat)

test_that("draw_thompson correctly draws arms for a LinTS agent with contextual vectors", {
  # Create a sample model
  model <- LinTSModel(K = 5, p = 3,
                      floor_start = 1,
                      floor_decay = 0.9,
                      num_mc = 100,
                      is_contextual = TRUE)

  # Generate sample data
  start <- 1
  end <- 10
  xs <- matrix(rnorm(30), ncol = 3)

  # Call the draw_thompson function
  result <- draw_thompson(model = model, start = start, end = end, xs = xs)

  # Perform assertions to check if the arms and probabilities are correctly drawn
  expect_equal(length(result$w), end - start + 1)
  expect_true(all(result$w <= model$K))

  expect_equal(dim(result$ps), c(nrow(xs), model$K))
  expect_true(all(abs(result$ps -
                        min((model$floor_start / (model$floor_decay * start)),
                            1/model$K)) < 1e-15))
})

test_that("draw_thompson correctly draws arms for a non-contextual TS agent", {
  # Create a sample model
  model <- LinTSModel(K = 5, p = 3, floor_start = 1, floor_decay = 0.9, num_mc = 100, is_contextual = FALSE)

  # Generate sample data
  start <- 1
  end <- 10

  # Call the draw_thompson function
  result <- draw_thompson(model = model, start = start, end = end)

  # Perform assertions to check if the arms and probabilities are correctly drawn
  expect_equal(length(result$w), end - start + 1)
  expect_true(all(result$w <= model$K))

  expect_equal(length(result$ps), model$K)
  expect_true(all(abs(result$ps -
                    min((model$floor_start / (model$floor_decay * start)),
                        1/model$K)) < 1e-15))

})
