require(testthat)
context("patch_permute")

test_that("the patch_permute constructor and get_patch_params function work", {

  perm <- c(2L, 5L)
  expect_error(patch_permute(perm), regexp = "setequal")

  perm <- 1:5
  target <- patch_permute(perm)
  expect_true(is_patch(target, allow_composed = FALSE))

  perm <- as.integer(c(2, 4, 1, 3, 5))
  target <- patch_permute(perm)
  expect_true(is_patch(target, allow_composed = FALSE))

  expect_identical(get_patch_params(target)[["perm"]], expected = perm)
})

test_that("the print_patch_params method works", {

  perm <- as.integer(c(2, 4, 1, 3, 5))
  indent <- "  "
  target <- patch_permute(perm)
  # old: expected <- "perm: 1 2 3 4 5\n      3 1 4 2 5"
  expected <- paste(paste0(indent, "1 -> 3"),
                    paste0(indent, "2 -> 1"),
                    paste0(indent, "3 -> 4"),
                    paste0(indent, "4 -> 2"),
                    paste0(indent, "5 -> 5"),
                    sep = "\n")
  expect_identical(print_patch_params(target, indent = indent, all_cols = TRUE),
                   expected = expected)

  # old: expected <- "perm: 1 2 3 4\n      3 1 4 2"
  expected <- paste(paste0(indent, "1 -> 3"),
                    paste0(indent, "2 -> 1"),
                    paste0(indent, "3 -> 4"),
                    paste0(indent, "4 -> 2"),
                    sep = "\n")
  expect_identical(print_patch_params(target, all_cols = FALSE), expected = expected)
})

test_that("patch function application works", {

  df <- data.frame("a" = 1:10, "b" = 11:20, "c"=21:30)

  perm <- as.integer(c(2, 1, 3, 4))
  target <- patch_permute(perm)

  expect_error(target(df), regexp = "is_compatible_columns")

  perm <- as.integer(c(2, 1, 3))
  target <- patch_permute(perm)

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

test_that("the sample_patch_permute function works", {

  df <- mtcars
  result <- sample_patch_permute(df)
  expect_true(is_patch(result))
  expect_equal(patch_type(result), "permute")

  # Test the n argument.
  n <- 3L
  sink <- sapply(1:20, FUN = function(i) {
    set.seed(sample.int(10^6, size = 1))
    result <- sample_patch_permute(df, n = n)
    expect_true(sum(get_patch_params(result)[["perm"]] != 1:ncol(df)) <= n)
  })

  # Test the is_fixed argument.
  result <- sample_patch_permute(df, is_fixed = c(rep(TRUE, ncol(df) - 2), FALSE, FALSE))
  expect_true(is_patch(result))
  expect_equal(patch_type(result), "permute")
  expect_equal(get_patch_params(result)[["perm"]], c(1:(ncol(df) - 2), ncol(df), ncol(df) - 1))

})
