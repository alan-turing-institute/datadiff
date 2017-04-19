require(testthat)
context("gen_patch_scale")

test_that("the gen_patch_scale function works", {

  set.seed(2222)

  df1 <- data.frame(x = rnorm(50, sd = 2))
  df2 <- data.frame(x = rnorm(100, sd = 10))

  result <- gen_patch_scale(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result))
  expect_identical(patch_type(result), expected = "scale")
  expect_true(ks(result(df1)[[1L]], df2[[1L]]) < ks(df1[[1L]], df2[[1L]]))

  df1 <- data.frame(x = rexp(50, rate = 1))
  df2 <- data.frame(x = rexp(100, rate = 4))

  result <- gen_patch_scale(df1, col1 = 1L, df2 = df2)
  expect_true(ks(result(df1)[[1L]], df2[[1L]]) < ks(df1[[1L]], df2[[1L]]))

  df1 <- data.frame(x = rnorm(50, sd = 2))
  df2 <- data.frame(x = rexp(100))

  result <- gen_patch_scale(df1, col1 = 1L, df2 = df2)

  # The boundary at zero prohibits mismatch reduction in this case.
  expect_true(ks(result(df1)[[1L]], df2[[1L]]) == ks(df1[[1L]], df2[[1L]]))

  # Shifting the boundary to -2 provides an opportunity.
  df2[[1]] <- df2[[1]] - 2
  result <- gen_patch_scale(df1, col1 = 1L, df2 = df2)
  expect_true(ks(result(df1)[[1L]], df2[[1L]]) < ks(df1[[1L]], df2[[1L]]))

  df1 <- data.frame(x = rnorm(50, sd = 2))
  df2 <- data.frame(x = rlnorm(100))

  result <- gen_patch_scale(df1, col1 = 1L, df2 = df2)
  # The boundary at zero prohibits mismatch reduction in this case.
  expect_true(ks(result(df1)[[1L]], df2[[1L]]) == ks(df1[[1L]], df2[[1L]]))

  df1 <- data.frame(x = rexp(50))
  df2 <- data.frame(x = rlnorm(100))

  result <- gen_patch_scale(df1, col1 = 1L, df2 = df2)
  expect_true(ks(result(df1)[[1L]], df2[[1L]]) < ks(df1[[1L]], df2[[1L]]))
})

