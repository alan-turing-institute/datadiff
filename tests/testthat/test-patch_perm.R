require(testthat)
context("patch_perm")

test_that("the patch_perm constructor and get_patch_params function work", {

  perm <- c(2L, 5L)
  expect_error(patch_perm(perm), regexp = "setequal")

  perm <- 1:5
  target <- patch_perm(perm)
  expect_true(is_patch(target, allow_composed = FALSE))

  perm <- as.integer(c(2, 4, 1, 3, 5))
  target <- patch_perm(perm)
  expect_true(is_patch(target, allow_composed = FALSE))

  expect_identical(get_patch_params(target)[["perm"]], expected = perm)
})

test_that("the print_patch_params method works", {

  perm <- as.integer(c(2, 4, 1, 3, 5))
  target <- patch_perm(perm)
  expected <- "perm: 1 2 3 4 5\n      3 1 4 2 5"
  expect_identical(print_patch_params(target), expected = expected)
})

test_that("patch function application works", {

  df <- data.frame("a" = 1:10, "b" = 11:20, "c"=21:30)

  perm <- as.integer(c(2, 1, 3, 4))
  target <- patch_perm(perm)

  expect_error(target(df), regexp = "is_compatible_columns")

  perm <- as.integer(c(2, 1, 3))
  target <- patch_perm(perm)

  expect_identical(target(df), expected = df[perm])
  expect_identical(colnames(target(df)), expected = c("b", "a", "c"))

  # Bugfix 12/05/2017. Test with duplicated column names.
  df <- data.frame("a" = 1:10, "b" = 11:20, "c"=21:30)
  colnames(df) <- c("a", "b", "a")

  expected <- df[perm]
  colnames(expected) <- colnames(df)[perm]
  expect_identical(target(df), expected = expected)
  expect_identical(colnames(target(df)), expected = c("b", "a", "a"))
})
