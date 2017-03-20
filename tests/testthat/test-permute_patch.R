require(testthat)
context("permute_patch methods")

test_that("the patch constructor and patch_type methods work", {

  ## Permute patch.
  params <- c(2L, 5L)
  target <- patch(params)

  expect_true(is(target, "patch"))
  expect_true(is(target, datadiff:::TYPE_PERMUTE))
  expect_equal(patch_type(target), expected = datadiff:::TYPE_PERMUTE)
  expect_equal(patch_params(target), expected = params)
})

test_that("the is_compatible method works", {

  ## Permute patch.
  params <- c(2L, 5L)
  target <- patch(params)

  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))
  expect_true(is_compatible(target, df))

  df <- as.data.frame(matrix(1:20, nrow = 5, ncol = 4))
  expect_false(is_compatible(target, df))

})

test_that("patch function application works", {

  ## Permute patch.
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))

  params <- c(2L, 5L)
  target <- patch(params)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(result[[1]], expected = df[[1]])
  expect_equal(result[[2]], expected = df[[5]])
  expect_equal(result[[3]], expected = df[[3]])
  expect_equal(result[[4]], expected = df[[4]])
  expect_equal(result[[5]], expected = df[[2]])

  # Apply to an incompatible data frame.
  df <- data.frame(col1 = 1:12, col2 = 12:1)
  expect_error(target(df), regexp = "Incompatible")
})
