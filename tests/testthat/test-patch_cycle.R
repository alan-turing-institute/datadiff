require(testthat)
context("patch_cycle")

test_that("the patch_cycle constructor and get_patch_params function work", {

  ## Cycle columns by index.
  target <- patch_cycle(c(2L, 5L))

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_cycle"))
  expect_equal(get_patch_params(target), expected = list("cols" = c(2L, 5L)))

  ## Cycle columns by name.
  target <- patch_cycle(c("mpg", "gear"))

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_cycle"))
  expect_equal(get_patch_params(target), list("cols" = c("mpg", "gear")))
})

test_that("patch function application works", {

  ## Cycle by column index.
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))

  target <- patch_cycle(c(2L, 5L))
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(result[[1]], expected = df[[1]])
  expect_equal(result[[2]], expected = df[[5]])
  expect_equal(result[[3]], expected = df[[3]])
  expect_equal(result[[4]], expected = df[[4]])
  expect_equal(result[[5]], expected = df[[2]])

  target <- patch_cycle(c(2L, 5L, 3L))
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(result[[1]], expected = df[[1]])
  expect_equal(result[[2]], expected = df[[5]])
  expect_equal(result[[3]], expected = df[[2]])
  expect_equal(result[[4]], expected = df[[4]])
  expect_equal(result[[5]], expected = df[[3]])

  ## Cycle by column name.
  names(df) <- letters[1:5]

  target <- patch_cycle(c("b", "e"))
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(names(result), expected = c("a", "e", "c", "d", "b"))
  expect_equal(result[[1]], expected = df[[1]])
  expect_equal(result[[2]], expected = df[[5]])
  expect_equal(result[[3]], expected = df[[3]])
  expect_equal(result[[4]], expected = df[[4]])
  expect_equal(result[[5]], expected = df[[2]])

  target <- patch_cycle(c(2L, 5L, 3L))
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(names(result), expected = c("a", "e", "b", "d", "c"))
  expect_equal(result[[1]], expected = df[[1]])
  expect_equal(result[[2]], expected = df[[5]])
  expect_equal(result[[3]], expected = df[[2]])
  expect_equal(result[[4]], expected = df[[4]])
  expect_equal(result[[5]], expected = df[[3]])

  # Apply to an incompatible data frame.
  expect_error(target(df[, 1:4]), regexp = "is_compatible_columns.*not TRUE")
})
