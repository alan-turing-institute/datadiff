require(testthat)
context("gen_patch_encode")

test_that("the gen_patch_encode function works", {

  prob1 <- c(0.15, 0.1, 0.25, 0.05, 0.45)
  prob2 <- c(0.25, 0.45, 0.15, 0.05, 0.1)

  df1 <- data.frame("v1" = sample.int(5, size=1000, replace=TRUE, prob=prob1))
  df2 <- data.frame("v2" = sample.int(5, size=1000, replace=TRUE, prob=prob2))

  result <- gen_patch_encode(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result))
  expect_identical(patch_type(result), expected = "encode")

  expected <- c("1" = 3, "2" = 5, "3" = 1, "4" = 4, "5" = 2)
  expect_equal(get_patch_params(result)[["encoding"]], expected)

  # Test with non-integer, non-factor data. Take care with tabulate in this case.
  df1 <- data.frame("v1" = sample(letters[1:5], size=1000, replace=TRUE,
                                  prob=prob1), stringsAsFactors = FALSE)
  df2 <- data.frame("v2" = sample(letters[1:5], size=1000, replace=TRUE,
                                  prob=prob2), stringsAsFactors = FALSE)

  result <- gen_patch_encode(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result))
  expect_identical(patch_type(result), expected = "encode")

  expected <- c("a" = "c", "b" = "e", "c" = "a", "d" = "d", "e" = "b")
  expect_equal(get_patch_params(result)[["encoding"]], expected)

  ## Test with factor columns.
  # Both factors.
  df1 <- data.frame("v1" = sample(letters[1:5], size=1000, replace=TRUE,
                                  prob=prob1), stringsAsFactors = TRUE)
  df2 <- data.frame("v2" = sample(letters[1:5], size=1000, replace=TRUE,
                                  prob=prob2), stringsAsFactors = TRUE)

  result <- gen_patch_encode(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result))
  expect_identical(patch_type(result), expected = "encode")

  expected <- c("a" = "c", "b" = "e", "c" = "a", "d" = "d", "e" = "b")
  expect_equal(get_patch_params(result)[["encoding"]], expected)

  # Only col1 a factor.
  df1 <- data.frame("v1" = sample(letters[1:5], size=1000, replace=TRUE,
                                  prob=prob1), stringsAsFactors = TRUE)
  df2 <- data.frame("v2" = sample(letters[1:5], size=1000, replace=TRUE,
                                  prob=prob2), stringsAsFactors = FALSE)

  result <- gen_patch_encode(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result))
  expect_identical(patch_type(result), expected = "encode")

  expected <- c("a" = "c", "b" = "e", "c" = "a", "d" = "d", "e" = "b")
  expect_equal(get_patch_params(result)[["encoding"]], expected)

  # Only col2 a factor.
  df1 <- data.frame("v1" = sample(letters[1:5], size=1000, replace=TRUE,
                                  prob=prob1), stringsAsFactors = FALSE)
  df2 <- data.frame("v2" = sample(letters[1:5], size=1000, replace=TRUE,
                                  prob=prob2), stringsAsFactors = TRUE)

  result <- gen_patch_encode(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result))
  expect_identical(patch_type(result), expected = "encode")

  expected <- c("a" = "c", "b" = "e", "c" = "a", "d" = "d", "e" = "b")
  expect_equal(get_patch_params(result)[["encoding"]], expected)

  ## Test with different types.
  df1 <- data.frame("v1" = sample.int(5, size=1000, replace=TRUE, prob=prob1))
  df2 <- data.frame("v2" = sample(letters[1:5], size=1000, replace=TRUE,
                                  prob=prob2), stringsAsFactors = FALSE)

  result <- gen_patch_encode(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result))
  expect_identical(patch_type(result), expected = "encode")

  expected <- c("1" = "c", "2" = "e", "3" = "a", "4" = "d", "5" = "b")
  expect_equal(get_patch_params(result)[["encoding"]], expected)

  ## Test with invalid encodings.
  df1 <- data.frame("v1" = sample.int(5, size=1000, replace=TRUE, prob=prob1))
  df2 <- data.frame("v2" = sample.int(3, size=1000, replace=TRUE))

  expect_error(gen_patch_encode(df1, col1 = 1L, df2 = df2),
               regexp = "Insufficient target codes")

  df1 <- data.frame("v1" = sample.int(5, size=1000, replace=TRUE, prob=prob1))
  df2 <- data.frame("v2" = rnorm(1000))

  expect_error(gen_patch_encode(df1, col1 = 1L, df2 = df2),
               regexp = "Encodings require discrete data")

  # Test in the presence of NA values.
  df1 <- data.frame("v1" = sample.int(5, size=1000, replace=TRUE, prob=prob1))
  df2 <- data.frame("v2" = sample.int(5, size=1000, replace=TRUE, prob=prob2))
  df1[sample.int(1000, size = 10), 1] <- NA

  result <- gen_patch_encode(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result))
  expect_identical(patch_type(result), expected = "encode")

  expected <- c("1" = 3, "2" = 5, "3" = 1, "4" = 4, "5" = 2)
  expect_equal(get_patch_params(result)[["encoding"]], expected)

  # NA values in the target.
  df1 <- data.frame("v1" = sample.int(5, size=1000, replace=TRUE, prob=prob1))
  df2 <- data.frame("v2" = sample.int(5, size=1000, replace=TRUE, prob=prob2))
  df2[sample.int(1000, size = 10), 1] <- NA

  result <- gen_patch_encode(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result))
  expect_identical(patch_type(result), expected = "encode")

  expected <- c("1" = 3, "2" = 5, "3" = 1, "4" = 4, "5" = 2)
  expect_equal(get_patch_params(result)[["encoding"]], expected)

  # All NA values.
  df1[, 1] <- NA
  expect_error(gen_patch_encode(df1, col1 = 1L, df2 = df2))

  ## Test with pre- and post-encodings of different.
  prob1 <- c(0.2, 0.1, 0.7)
  prob2 <- c(0.1, 0.6, 0.05, 0.2, 0.05)

  df1 <- data.frame("v1" = sample.int(3, size=1000, replace=TRUE, prob=prob1))
  df2 <- data.frame("v2" = sample.int(5, size=1000, replace=TRUE, prob=prob2))

  result <- gen_patch_encode(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result))
  expect_identical(patch_type(result), expected = "encode")

  expected <- c("1" = 4, "2" = 1, "3" = 2)
  expect_equal(get_patch_params(result)[["encoding"]], expected)

  # Different lengths _and_ types.
  df1 <- data.frame("v1" = sample.int(3, size=1000, replace=TRUE, prob=prob1))
  df2 <- data.frame("v2" = sample(letters[1:5], size=1000, replace=TRUE,
                                  prob=prob2), stringsAsFactors = FALSE)

  result <- gen_patch_encode(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result))
  expect_identical(patch_type(result), expected = "encode")

  expected <- c("1" = "d", "2" = "a", "3" = "b")
  expect_equal(get_patch_params(result)[["encoding"]], expected)

})
