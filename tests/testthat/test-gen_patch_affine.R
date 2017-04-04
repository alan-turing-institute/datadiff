require(testthat)
context("gen_patch_affine")

test_that("the gen_patch_affine function works", {

  set.seed(2222)

  df <- data.frame(x = rnorm(1000, mean = 7, sd = 3))
  y <- rnorm(1000, mean = 17, sd = 5)

  result <- gen_patch_affine(df, cols = 1L, y = y)

  expect_false(is_patch(result, allow_composed = FALSE))
  expect_true(is_patch(result, allow_composed = TRUE))
  expect_identical(patch_type(result), c("shift", "scale"))

  expect_equal(get_patch_params(result)[[1]][["shift"]],
               expected = ((3/5)*17) - 7, tolerance = 0.5)
  expect_equal(get_patch_params(result)[[2]][["scale_factor"]], expected = 5/3,
               tolerance = 0.1)

  # Check that the sample statistics for the transformed and target data match.
  expect_equal(mean(result(df)[[1]]), mean(y))
  expect_equal(stats::mad(result(df)[[1]]), stats::mad(y))

  ## Test the robust argument.
  result <- gen_patch_affine(df, cols = 1L, y = y, robust = FALSE)

  expect_false(is_patch(result, allow_composed = FALSE))
  expect_true(is_patch(result, allow_composed = TRUE))
  expect_identical(patch_type(result), c("shift", "scale"))

  expect_equal(get_patch_params(result)[[1]][["shift"]],
               expected = ((3/5)*17) - 7, tolerance = 0.5)
  expect_equal(get_patch_params(result)[[2]][["scale_factor"]], expected = 5/3,
               tolerance = 0.1)

  # Check that the sample statistics for the transformed and target data match.
  expect_equal(mean(result(df)[[1]]), mean(y))
  expect_equal(sd(result(df)[[1]]), sd(y))

})
