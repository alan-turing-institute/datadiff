require(testthat)
context("gen_patch_affine")

test_that("the gen_patch_affine function works", {

  set.seed(2222)

  df1 <- data.frame(x = rnorm(1000, mean = 0, sd = 1))
  df2 <- data.frame(x = rnorm(1000, mean = 4, sd = 3))

  result <- gen_patch_affine(df1, col1 = 1L, df2 = df2)

  expect_false(is_patch(result, allow_composed = FALSE))
  expect_true(is_patch(result, allow_composed = TRUE))

  expect_true(ks(result(df1)[[1L]], df2[[1L]]) < 1/2 * ks(df1[[1L]], df2[[1L]]))

  expect_identical(patch_type(result), c("scale", "shift"))

  # In this simple case the optimised parameters agree with those expected,
  # given the underlying distributions.
  expect_equal(get_patch_params(result)[[1]][["scale_factor"]],
               expected = 3, tolerance = 0.1)
  expect_equal(get_patch_params(result)[[2]][["shift"]],
               expected = 4, tolerance = 0.1)

  df1 <- data.frame(x = rnorm(1000, mean = 7, sd = 3))
  df2 <- data.frame(x = rnorm(1000, mean = 17, sd = 5))

  result <- gen_patch_affine(df1, col1 = 1L, df2 = df2)

  expect_false(is_patch(result, allow_composed = FALSE))
  expect_true(is_patch(result, allow_composed = TRUE))

  expect_true(ks(result(df1)[[1L]], df2[[1L]]) < 1/2 * ks(df1[[1L]], df2[[1L]]))

  # TODO: test with samples from various distributions.
})
