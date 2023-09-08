library(testthat)

test_that("LinTSModel creates the correct model structure", {
  model <- LinTSModel(K = 5,
                      p = 3,
                      floor_start = 1,
                      floor_decay = 0.9,
                      num_mc = 100,
                      is_contextual = TRUE)

  expect_type(model, "list")
  expect_equal(length(model), 11)

  expect_equal(model$num_mc, 100)
  expect_equal(model$K, 5)
  expect_equal(model$p, 3)
  expect_equal(model$floor_start, 1)
  expect_equal(model$floor_decay, 0.9)

  expect_equal(length(model$y), 5)
  expect_equal(length(model$ps), 5)

  if (model$is_contextual) {
    expect_equal(nrow(model$mu), 5)
    expect_equal(ncol(model$mu), 4)
    expect_equal(dim(model$V), c(5, 4, 4))
    expect_equal(length(model$X), 5)
  } else {
    expect_false(exists("mu", envir = as.environment(model)))
    expect_false(exists("V", envir = as.environment(model)))
    expect_false(exists("X", envir = as.environment(model)))
  }
})
