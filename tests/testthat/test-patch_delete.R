require(testthat)
context("patch_delete")

test_that("the patch_delete constructor and get_patch_params functions work", {

  ## Delete columns by index.
  target <- patch_delete(c(2L, 5L))

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_delete"))
  expect_equal(get_patch_params(target), expected = list("cols" = c(2L, 5L)))

  ## Delete columns by name.
  target <- patch_delete(c("mpg", "gear"))

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_delete"))
  expect_equal(get_patch_params(target), list("cols" = c("mpg", "gear")))
})

test_that("patch function application works", {

  ## Delete by column index.
  df <- data.frame(col1 = 1:12, col2 = 12:1)

  cols <- 2L
  target <- patch_delete(cols)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df) - 1)
  expect_equal(result[[1]], df[[1]])

  # Test the case where the result is empty.
  cols <- c(1L, 2L)
  target <- patch_delete(cols)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df) - 2)

  # Apply to an incompatible data frame.
  cols <- 3L
  target <- patch_delete(cols)
  expect_error(target(df), regexp = "is_compatible_columns.*not TRUE")

  ## Delete by column name.
  df <- data.frame(col1 = 1:12, col2 = 12:1)
  names(df) <- c("a", "b")

  cols <- "b"
  target <- patch_delete(cols)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df) - 1)
  expect_equal(names(result), expected = "a")
  expect_equal(result[["a"]], df[["a"]])

  # Test the case where the result is empty.
  cols <- names(df)
  target <- patch_delete(cols)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df) - 2)

  # Apply to an incompatible data frame.
  cols <- "c"
  target <- patch_delete(cols)
  expect_error(target(df), regexp = "is_compatible_columns.*not TRUE")
})
