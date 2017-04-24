require(testthat)
context("gen_patch_scale")

test_that("the gen_patch_scale function works", {

  set.seed(2222)

  df1 <- data.frame(x = rnorm(50, sd = 2))
  df2 <- data.frame(x = rnorm(100, sd = 10))

  result <- gen_patch_scale(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result))
  expect_identical(patch_type(result), expected = "scale")
  expect_true(ks(result(df1)[[1L]], df2[[1L]]) < 1/2 * ks(df1[[1L]], df2[[1L]]))

  df1 <- data.frame(x = rexp(50, rate = 1))
  df2 <- data.frame(x = rexp(100, rate = 4))

  result <- gen_patch_scale(df1, col1 = 1L, df2 = df2)
  expect_true(ks(result(df1)[[1L]], df2[[1L]]) < 1/2 * ks(df1[[1L]], df2[[1L]]))

  df1 <- data.frame(x = rnorm(50, sd = 2))
  df2 <- data.frame(x = rexp(100))

  result <- gen_patch_scale(df1, col1 = 1L, df2 = df2)
  # The boundary at zero prohibits any significant mismatch reduction in this case.
  expect_equal(ks(result(df1)[[1L]], df2[[1L]]), ks(df1[[1L]], df2[[1L]]))

  df1 <- data.frame(x = rnorm(50, sd = 2))
  df2 <- data.frame(x = rlnorm(100))

  result <- gen_patch_scale(df1, col1 = 1L, df2 = df2)
  # The boundary at zero prohibits any significant mismatch reduction in this case.
  expect_equal(ks(result(df1)[[1L]], df2[[1L]]), ks(df1[[1L]], df2[[1L]])
               , tolerance = 0.05)

  df1 <- data.frame(x = rexp(50))
  df2 <- data.frame(x = rlnorm(100))

  result <- gen_patch_scale(df1, col1 = 1L, df2 = df2)
  expect_true(ks(result(df1)[[1L]], df2[[1L]]) < 1/2 * ks(df1[[1L]], df2[[1L]]))

  # Test the interval argument passed to optimise in the presence of small values
  # and mixed signs.

  df1 <- data.frame(x = rnorm(50, sd = 2))
  df2 <- data.frame(x = rnorm(100, mean = -1, sd = 10))

  result <- gen_patch_scale(df1, col1 = 1L, df2 = df2)
  expect_true(ks(result(df1)[[1L]], df2[[1L]]) < 1/2 * ks(df1[[1L]], df2[[1L]]))

  df1 <- data.frame(x = rnorm(100, mean = -1, sd = 10))
  df2 <- data.frame(x = rnorm(50, sd = 2))

  result <- gen_patch_scale(df1, col1 = 1L, df2 = df2)
  expect_true(ks(result(df1)[[1L]], df2[[1L]]) < 1/2 * ks(df1[[1L]], df2[[1L]]))

})

