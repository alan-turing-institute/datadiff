require(testthat)
context("delete.patch methods")

test_that("the patch constructor and patch_type methods work", {

  ## Delete column patch.
  params <- -2L
  target <- patch(params)

  expect_true(is(target, "patch"))
  expect_true(is(target, datadiff:::TYPE_DELETE))
  expect_equal(patch_type(target), expected = datadiff:::TYPE_DELETE)
  expect_equal(patch_params(target), expected = params)
})

test_that("the is_compatible method works", {

  ## Delete column patch.
  params <- -2L
  target <- patch(params)

  df <- data.frame(col1 = 1:12, col2 = 12:1)
  expect_true(is_compatible(target, df))

  params <- -c(1L, 2L)
  target <- patch(params)

  df <- data.frame(col1 = 1:12, col2 = 12:1)
  expect_true(is_compatible(target, df))

  df <- data.frame(col1 = 1:12)
  expect_false(is_compatible(target, df))
})

test_that("patch function application works", {

  ## Delete column patch.
  df <- data.frame(col1 = 1:12, col2 = 12:1)

  params <- -2L
  target <- patch(params)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df) - 1)
  expect_equal(result[[1]], df[[1]])

  params <- -1L
  target <- patch(params)

  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df) - 1)
  expect_equal(result[[1]], df[[2]])

  # Test the case where the result is empty.
  params <- -c(1L, 2L)
  target <- patch(params)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df) - 2)

  # Apply to an incompatible data frame.
  params <- -3L
  target <- patch(params)
  expect_error(target(df), regexp = "Incompatible")
})
