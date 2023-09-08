library(testthat)

test_that("update_thompson updates the model parameters correctly", {
  # Create a sample model
  model <- LinTSModel(K = 3,
                      p = 3,
                      floor_start = 1,
                      floor_decay = 0.9,
                      num_mc = 100,
                      is_contextual = TRUE)

  # Generate sample data
  ws <- c(3, 2, 1)
  yobs <- c(1, 0, 1)
  xs <- matrix(rnorm(9), ncol = 3)

  # Call the update_thompson function
  updated_model <- update_thompson(ws = ws, yobs = yobs, model = model, xs = xs)

  # Perform assertions to check if the parameters are updated correctly
  expect_equal(nrow(updated_model$X[[3]]), 1)
  expect_equal(updated_model$X[[3]][1, ], xs[1, ])

  expect_equal(nrow(updated_model$X[[2]]), 1)
  expect_equal(updated_model$X[[2]][1, ], xs[2, ])

  expect_equal(nrow(updated_model$X[[1]]), 1)
  expect_equal(updated_model$X[[1]][1, ], xs[3, ])

  expect_equal(nrow(updated_model$y[[3]]), 1)
  expect_equal(updated_model$y[[3]][1, ], yobs[1])

  expect_equal(nrow(updated_model$y[[2]]), 1)
  expect_equal(updated_model$y[[2]][1, ], yobs[2])

  expect_equal(nrow(updated_model$y[[1]]), 1)
  expect_equal(updated_model$y[[1]][1, ], yobs[3])
})
