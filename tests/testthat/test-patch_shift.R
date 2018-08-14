require(testthat)
context("patch_shift")

test_that("the patch_shift constructor and get_patch_params function work", {

  ## Shift columns by index.
  target <- patch_shift(c(2L, 5L), shift = 2)

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_shift"))

  expected <- list("cols" = c(2L, 5L), "shift" = 2)
  expect_equal(get_patch_params(target), expected = expected)

  ## Shift columns by name.
  target <- patch_shift(c("mpg", "gear"), shift = 2)

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_shift"))

  expected <- list("cols" = c("mpg", "gear"), "shift" = 2)
  expect_equal(get_patch_params(target), expected = expected)
})

test_that("patch function application works", {

  ## Shift by column index.
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))

  target <- patch_shift(c(2L, 5L), shift = 2)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(result[[1]], expected = df[[1]])
  expect_equal(result[[2]], expected = 2 + df[[2]])
  expect_equal(result[[3]], expected = df[[3]])
  expect_equal(result[[4]], expected = df[[4]])
  expect_equal(result[[5]], expected = 2 + df[[5]])

  ## Shift by column name.
  names(df) <- letters[1:5]

  target <- patch_shift(c("b", "e"), shift = 2)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(names(result), expected = names(df))
  expect_equal(result[["a"]], expected = df[["a"]])
  expect_equal(result[["b"]], expected = 2 + df[["b"]])
  expect_equal(result[["c"]], expected = df[["c"]])
  expect_equal(result[["d"]], expected = df[["d"]])
  expect_equal(result[["e"]], expected = 2 + df[["e"]])

  # Apply to an incompatible data frame.
  expect_error(target(df[, 1:4]), regexp = "is_compatible_columns.*not TRUE")
})

test_that("the sample_patch_shift function works", {

  df <- mtcars
  result <- sample_patch_shift(df)
  expect_true(is_patch(result))
  expect_equal(patch_type(result), "shift")

  result <- sample_patch_shift(df, mean = 20)
  expect_true(is_patch(result))

  # Test that the default shift is 0.
  result <- sample_patch_shift(df, sd = 0)
  expect_true(is_patch(result))
  expect_equal(get_patch_params(result)[["shift"]], expected = 0)

  result <- sample_patch_shift(df, rdist = rnorm, sd = 0)
  expect_true(is_patch(result))
  expect_equal(get_patch_params(result)[["shift"]], expected = 0)

  result <- sample_patch_shift(df, mean = 5, sd = 0)
  expect_true(is_patch(result))
  expect_equal(get_patch_params(result)[["shift"]], expected = 5)

  # Test with a different distribution function.
  result <- sample_patch_shift(df, rdist = rexp)
  expect_true(is_patch(result))

  result <- sample_patch_shift(df, rdist = rexp, rate = 3)
  expect_true(is_patch(result))

  # Test the relative_shift argument.
  expect_error(sample_patch_shift(df, relative_shift = 2, mean = 20),
               regexp = "matched by multiple actual arguments")

  result <- sample_patch_shift(df, relative_shift = 2)
  expect_true(is_patch(result))
})
