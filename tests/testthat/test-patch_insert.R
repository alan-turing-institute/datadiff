require(testthat)
context("patch_insert")

test_that("the patch_insert constructor and get_patch_params functions work", {

  target <- patch_insert(2L, data = data.frame(rep(NA, 10)))

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_insert"))
  expected <- list("insertion_point"=2L, "data"=data.frame(rep(NA, 10)))
  expect_equal(get_patch_params(target), expected)

  expect_error(patch_insert(c(2L, 5L), data = data.frame(rep(NA, 10))))
})

test_that("patch function application works", {

  ## Delete by column index.
  df <- data.frame(col1 = 1:10, col2 = 11:20)
  expect_equal(length(df), expected = 2)

  data <- data.frame("x" = rep(NA, 10))

  target <- patch_insert(0L, data = data)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(length(result), expected = 3)
  expect_identical(names(result), c(names(data), names(df)))
  expect_identical(result[1], data)
  expect_identical(result[2:3], df)

  target <- patch_insert(1L, data = data)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(length(result), expected = 3)
  expect_identical(names(result), c(names(df)[1], names(data), names(df)[2]))
  expect_identical(result[2], data)
  expect_identical(result[c(1,3)], df)

  target <- patch_insert(2L, data = data)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(length(result), expected = 3)
  expect_identical(names(result), c(names(df), names(data)))
  expect_identical(result[3], data)
  expect_identical(result[1:2], df)

  target <- patch_insert(3L, data = data)
  expect_error(target(df), regexp = "is_compatible_columns")

  # Test with a character insertion point and multi-column data.
  data <- data.frame("x" = rep(NA, 10), "y" = 10:1)

  target <- patch_insert("col1", data = data)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(length(result), expected = 4)
  expect_identical(names(result), c("col1", names(data), "col2"))
  expect_identical(result[2:3], data)
  expect_identical(result[c(1, 4)], df)

  target <- patch_insert("col2", data = data)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(length(result), expected = 4)
  expect_identical(names(result), c(names(df), names(data)))
  expect_identical(result[3:4], data)
  expect_identical(result[1:2], df)

  # Test with conflicting column names.
  data <- data.frame("x" = rep(NA, 10), "col2" = 10:1)

  target <- patch_insert("col1", data = data)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(length(result), expected = 4)
  expect_identical(names(result), c("col1", names(data), "col2"))
  expect_identical(result[2:3], data)
  expect_identical(result[c(1, 4)], df)

  data <- data.frame("x" = rep(NA, 10), "col2" = 10:1)

  target <- patch_insert("col2", data = data)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(length(result), expected = 4)
  expect_identical(names(result), c(names(df), names(data)))
  expect_identical(result[3:4], data)
  expect_identical(result[1:2], df)

  # Apply to an incompatible data frame.
  df <- data.frame(col1 = 1:10, col2 = 11:20)
  data <- data.frame("x" = rep(NA, 10), "y" = 10:1)

  target <- patch_insert(1L, data = data[1:9, ])
  expect_error(target(df), regexp = "nrow")

  target <- patch_insert("c", data = data)
  expect_error(target(df), regexp = "is_compatible_columns.*not TRUE")
})
