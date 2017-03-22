require(testthat)
context("patch_scale")

test_that("the patch_scale constructor and get_patch_params function work", {

  ## Scale columns by index.
  target <- patch_scale(c(2L, 5L), scale_factor = 2)

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_scale"))

  expected <- list("cols" = c(2L, 5L), "scale_factor" = 2)
  expect_equal(get_patch_params(target), expected = expected)

  ## Scale columns by name.
  target <- patch_scale(c("mpg", "gear"), scale_factor = 2)

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_scale"))

  expected <- list("cols" = c("mpg", "gear"), "scale_factor" = 2)
  expect_equal(get_patch_params(target), expected = expected)
})

test_that("patch function application works", {

  ## Scale by column index.
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))

  target <- patch_scale(c(2L, 5L), scale_factor = 2)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(result[[1]], expected = df[[1]])
  expect_equal(result[[2]], expected = 2 * df[[2]])
  expect_equal(result[[3]], expected = df[[3]])
  expect_equal(result[[4]], expected = df[[4]])
  expect_equal(result[[5]], expected = 2 * df[[5]])

  ## Scale by column name.
  names(df) <- letters[1:5]

  target <- patch_scale(c("b", "e"), scale_factor = 2)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(names(result), expected = names(df))
  expect_equal(result[["a"]], expected = df[["a"]])
  expect_equal(result[["b"]], expected = 2 * df[["b"]])
  expect_equal(result[["c"]], expected = df[["c"]])
  expect_equal(result[["d"]], expected = df[["d"]])
  expect_equal(result[["e"]], expected = 2 * df[["e"]])

  # Apply to an incompatible data frame.
  expect_error(target(df[, 1:4]), regexp = "is_compatible_columns.*not TRUE")
})
