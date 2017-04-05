require(testthat)
context("gen_patch_affine")

test_that("the gen_patch_affine function works", {

  set.seed(2222)

  df1 <- data.frame(x = rnorm(1000, mean = 7, sd = 3))
  df2 <- data.frame(x = rnorm(1000, mean = 17, sd = 5))

  ## shift then scale.
  result <- gen_patch_affine(df1, col1 = 1L, df2 = df2, shift_first = TRUE)

  expect_false(is_patch(result, allow_composed = FALSE))
  expect_true(is_patch(result, allow_composed = TRUE))

  expect_identical(patch_type(result), c("shift", "scale"))
  expect_equal(get_patch_params(result)[[1]][["shift"]],
               expected = ((3 / 5) * 17) - 7, tolerance = 0.5)
  expect_equal(get_patch_params(result)[[2]][["scale_factor"]], expected = 5/3,
               tolerance = 0.1)

  # Check that the sample statistics for the transformed and target data match.
  expect_equal(mean(result(df1)[[1L]]), mean(df2[[1L]]))
  expect_equal(stats::mad(result(df1)[[1L]]), stats::mad(df2[[1L]]))

  ## scale then shift.
  result <- gen_patch_affine(df1, col1 = 1L, df2 = df2, shift_first = FALSE)

  expect_identical(patch_type(result), c("scale", "shift"))
  expect_equal(get_patch_params(result)[[1]][["scale_factor"]], expected = 5/3,
               tolerance = 0.1)
  expect_equal(get_patch_params(result)[[2]][["shift"]],
               expected = 17 - 7 + (1 - (5/3)) * 7, tolerance = 0.5)

  # Check that the sample statistics for the transformed and target data match.
  expect_equal(mean(result(df1)[[1L]]), mean(df2[[1L]]))
  expect_equal(stats::mad(result(df1)[[1L]]), stats::mad(df2[[1L]]))

  #### Test with robust = FALSE.

  ## shift then scale.
  result <- gen_patch_affine(df1, col1 = 1L, df2 = df2, robust = FALSE,
                             shift_first = TRUE)

  expect_false(is_patch(result, allow_composed = FALSE))
  expect_true(is_patch(result, allow_composed = TRUE))

  expect_identical(patch_type(result), c("shift", "scale"))
  expect_equal(get_patch_params(result)[[1]][["shift"]],
               expected = ((3 / 5) * 17) - 7, tolerance = 0.5)
  expect_equal(get_patch_params(result)[[2]][["scale_factor"]], expected = 5/3,
               tolerance = 0.1)

  # Check that the sample statistics for the transformed and target data match.
  expect_equal(mean(result(df1)[[1L]]), mean(df2[[1L]]))
  expect_equal(sd(result(df1)[[1L]]), sd(df2[[1L]]))

  ## scale then shift.
  result <- gen_patch_affine(df1, col1 = 1L, df2 = df2, robust = FALSE,
                             shift_first = FALSE)

  expect_identical(patch_type(result), c("scale", "shift"))
  expect_equal(get_patch_params(result)[[1]][["scale_factor"]], expected = 5/3,
               tolerance = 0.1)
  expect_equal(get_patch_params(result)[[2]][["shift"]],
               expected = 17 - 7 + (1 - (5/3)) * 7, tolerance = 0.5)

  # Check that the sample statistics for the transformed and target data match.
  expect_equal(mean(result(df1)[[1L]]), mean(df2[[1L]]))
  expect_equal(sd(result(df1)[[1L]]), sd(df2[[1L]]))

})
