require(testthat)
context("gen_patch_rescale")

test_that("the gen_patch_rescale function works", {

  set.seed(.Machine$integer.max)
  df1 <- data.frame(x = rnorm(1000, mean = 0, sd = 1))
  df2 <- data.frame(x = rnorm(1000, mean = 4, sd = 3))

  result <- gen_patch_rescale(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result, allow_composed = FALSE))

  expect_identical(patch_type(result), "rescale")

  # In this simple case the optimised parameters agree with those expected,
  # given the underlying distributions (although the scale factor is negative).
  expect_equal(abs(get_patch_params(result)[["scale_factor"]]),
               expected = 3, tolerance = 0.2)
  expect_equal(get_patch_params(result)[["shift"]],
               expected = 4, tolerance = 0.02)

  set.seed(.Machine$integer.max)
  df1 <- data.frame(x = rnorm(1000, mean = 7, sd = 3))
  df2 <- data.frame(x = rnorm(1000, mean = 17, sd = 5))

  result <- gen_patch_rescale(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result, allow_composed = FALSE))

  # Suppose the units of temperature data are changed from celsius to fahrenheit.
  set.seed(.Machine$integer.max)
  f <- function(celsius) { 32 + (9/5) * celsius }
  df1 <- data.frame(x = rnorm(1000, mean = 100, sd = 2))
  df2 <- f(data.frame(x = rnorm(1000, mean = 100, sd = 2)))

  result <- gen_patch_rescale(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result, allow_composed = FALSE))

  # Again, the optimised parameters agree tolerably well with those expected
  # (again with a negative scale factor).
  expect_equal(get_patch_params(result)[["scale_factor"]],
               expected = 9/5, tolerance = 0.03)
  expect_equal(get_patch_params(result)[["shift"]],
               expected = 32, tolerance = 0.19)

  # Repeat the preceding test with only 150 samples.
  set.seed(.Machine$integer.max)
  df1 <- data.frame(x = rnorm(150, mean = 100, sd = 2))
  df2 <- f(data.frame(x = rnorm(150, mean = 100, sd = 2)))

  result <- gen_patch_rescale(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result, allow_composed = FALSE))

  # The parameter estimates are signficantly worse, but of the right order.
  expect_equal(get_patch_params(result)[["scale_factor"]],
               expected = 9/5, tolerance = 0.05)
  expect_equal(get_patch_params(result)[["shift"]],
               expected = 32, tolerance = 0.2)

  # TODO: test with samples from various distributions.
})

