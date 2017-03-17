require(testthat)
context("scale.patch methods")

test_that("the scale_patch_params function works", {

  column_indices <- c(2L, 5L, 7L)
  scale_factor <- 2
  result <- scale_patch_params(column_indices, scale_factor)

  expect_true(is.list(result))
  expect_equal(length(result), expected = 2)
  expect_true(setequal(names(result), c(datadiff:::COLUMNS, datadiff:::SCALE_FACTOR)))
  expect_equal(result[[datadiff:::COLUMNS]], expected = column_indices)
  expect_equal(result[[datadiff:::SCALE_FACTOR]], expected = scale_factor)

  column_indices <- c(2L, -5L, 7L)
  expect_error(scale_patch_params(column_indices, scale_factor), regexp = "Invalid column_indices")

  column_indices <- c(2L, 5L, 7L)
  scale_factor <- "2"
  expect_error(scale_patch_params(column_indices, scale_factor), regexp = "Invalid scale_factor")
  scale_factor <- 1:2
  expect_error(scale_patch_params(column_indices, scale_factor), regexp = "Invalid scale_factor")
})

test_that("the patch constructor and patch_type methods work", {

  ## Scale patch.
  params <- scale_patch_params(c(2L, 5L, 7L), 2)
  target <- patch(params)

  expect_true(is(target, "patch"))
  expect_true(is(target, datadiff:::TYPE_SCALE))
  expect_equal(patch_type(target), expected = datadiff:::TYPE_SCALE)
  expect_equal(patch_params(target), expected = params)
})

test_that("the is_compatible method works", {

  ## Scale patch.
  params <- scale_patch_params(c(2L, 5L), 2)
  target <- patch(params)

  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))
  expect_true(is_compatible(target, df))

  expect_false(is_compatible(target, df[, 1:4]))

  df[[2]] <- letters[1:4]
  expect_false(is_compatible(target, df))
})

test_that("patch function application works", {

  ## Scale patch.
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))

  params <- scale_patch_params(c(2L, 5L), 2)
  target <- patch(params)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(result[[1]], expected = df[[1]])
  expect_equal(result[[2]], expected = 2 * df[[2]])
  expect_equal(result[[3]], expected = df[[3]])
  expect_equal(result[[4]], expected = df[[4]])
  expect_equal(result[[5]], expected = 2 * df[[5]])

  # Apply to an incompatible data frame.
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))
  df[[2]] <- letters[1:4]

  # Incompatible due to non-numeric values in column 2.
  expect_error(target(df), regexp = "Incompatible")

  # Incompatible due to column index out of bounds.
  expect_error(target(df[, 1:4]), regexp = "Incompatible")
})
