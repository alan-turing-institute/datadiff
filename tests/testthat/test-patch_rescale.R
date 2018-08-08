require(testthat)
context("patch_rescale")

test_that("the patch_rescale constructor and get_patch_params function work", {

  ## Shift columns by index.
  target <- patch_rescale(c(2L, 5L), shift = 2)

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_rescale"))

  expected <- list("cols" = c(2L, 5L), "shift" = 2, "scale_factor" = 1)
  expect_equal(get_patch_params(target), expected = expected)

  ## Shift columns by name.
  target <- patch_rescale(c("mpg", "gear"), shift = 2)

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_rescale"))

  expected <- list("cols" = c("mpg", "gear"), "shift" = 2, "scale_factor" = 1)
  expect_equal(get_patch_params(target), expected = expected)

  ## Scale columns by index.
  target <- patch_rescale(c(2L, 5L), scale_factor = 2)

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_rescale"))

  expected <- list("cols" = c(2L, 5L), "shift" = 0, "scale_factor" = 2)
  expect_equal(get_patch_params(target), expected = expected)

  ## Scale columns by name.
  target <- patch_rescale(c("mpg", "gear"), scale_factor = 2)

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_rescale"))

  expected <- list("cols" = c("mpg", "gear"), "shift" = 0, "scale_factor" = 2)
  expect_equal(get_patch_params(target), expected = expected)

  ## Rescale (shift + scale) columns by index.
  target <- patch_rescale(c(2L, 5L), scale_factor = 2, shift = 10)

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_rescale"))

  expected <- list("cols" = c(2L, 5L), "shift" = 10, "scale_factor" = 2)
  expect_equal(get_patch_params(target), expected = expected)

  ## Rescale (shift + scale) columns by name.
  target <- patch_rescale(c("mpg", "gear"), shift = 5, scale_factor = 2)

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_rescale"))

  expected <- list("cols" = c("mpg", "gear"), "shift" = 5, "scale_factor" = 2)
  expect_equal(get_patch_params(target), expected = expected)})

test_that("patch function application works", {

  ## Shift by column index.
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))

  target <- patch_rescale(c(2L, 5L), shift = 2, scale_factor = 4)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(result[[1]], expected = df[[1]])
  expect_equal(result[[2]], expected = 2 + 4 * df[[2]])
  expect_equal(result[[3]], expected = df[[3]])
  expect_equal(result[[4]], expected = df[[4]])
  expect_equal(result[[5]], expected = 2 + 4 * df[[5]])

  ## Shift by column name.
  names(df) <- letters[1:5]

  target <- patch_rescale(c("b", "e"), shift = 2, scale_factor = 8)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(names(result), expected = names(df))
  expect_equal(result[["a"]], expected = df[["a"]])
  expect_equal(result[["b"]], expected = 2 + 8 * df[["b"]])
  expect_equal(result[["c"]], expected = df[["c"]])
  expect_equal(result[["d"]], expected = df[["d"]])
  expect_equal(result[["e"]], expected = 2 + 8 * df[["e"]])

  # Apply to an incompatible data frame.
  expect_error(target(df[, 1:4]), regexp = "is_compatible_columns.*not TRUE")
})

test_that("the sample_patch_rescale function works", {

  df <- mtcars
  result <- sample_patch_rescale(df)
  expect_true(is_patch(result))
  expect_equal(patch_type(result), "rescale")

  result <- sample_patch_rescale(df, mean_shift = 20, mean_scale = 2)
  expect_true(is_patch(result))

  # Test with a different distribution function.
  result <- sample_patch_rescale(df, rdist_shift = purrr::partial(rexp, rate = 3))
  expect_true(is_patch(result))

  result <- sample_patch_rescale(df,
                                 rdist_shift = purrr::partial(rexp, rate = 3),
                                 rdist_scale = purrr::partial(rlogis, location = 10))
  expect_true(is_patch(result))

  # Bugfix 21/11/2017:
  rdist_scale2 <- purrr::partial(stats::rnorm, mean = 2)
  sample_patch_rescale2 <- purrr::partial(sample_patch_rescale,
                                         rdist_scale = rdist_scale2)

  result <- sample_patch_rescale(df)
  expect_true(is_patch(result))
})
