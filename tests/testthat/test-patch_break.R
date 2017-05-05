require(testthat)
context("patch_break")

test_that("the patch_break constructor and get_patch_params function work", {

  cols <- c(2L, 5L)
  data <- data.frame(1, 2)
  target <- patch_break(cols, data)

  expect_true(is_patch(target, allow_composed = TRUE))
  expect_false(is_patch(target, allow_composed = FALSE))

  cols <- c(4L)
  data <- data.frame(1)
  target <- patch_break(cols, data)

  expect_true(is_patch(target, allow_composed = TRUE))
  expect_false(is_patch(target, allow_composed = FALSE))

  expect_equal(length(decompose_patch(target)), expected = 2)
  expect_true(is(decompose_patch(target)[[1]], "patch_insert"))
  expect_true(is(decompose_patch(target)[[2]], "patch_delete"))

  expect_equal(get_patch_params(decompose_patch(target)[[1]]),
               list("insertion_point" = 4L, "data" = data))
  expect_equal(get_patch_params(decompose_patch(target)[[2]]),
               list("cols" = cols))

})

test_that("patch function application works", {

  df <- data.frame("col1" = 1:12, "col2" = 12:1, "col3" = -1:-12)

  ## Identify columns by index
  cols <- 2L
  data <- data.frame("letters" = letters[12:1])
  target <- patch_break(cols, data)

  # Note that the column names in the data appear in the result.
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), ncol(df))
  expect_equal(colnames(result), expected = c("col1", "letters", "col3"))
  expect_equal(result[[1]], df[[1]])
  expect_equal(result[[2]], data[[1]])

  cols <- 1L
  data <- data.frame("letters" = letters[1:12])
  target <- patch_break(cols, data)

  # Note that the column names in the data appear in the result.
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), ncol(df))
  expect_equal(colnames(df), expected = c("col1", "col2", "col3"))
  expect_equal(colnames(result), expected = c("letters", "col2", "col3"))
  expect_equal(result[[1]], data[[1]])
  expect_equal(result[[2]], df[[2]])

  ## Test with multiple columns in the data.
  cols <- 1:2
  data <- data.frame("letters1" = letters[1:12], "letters2" = letters[12:1])
  target <- patch_break(cols, data)

  # Note that the column names in the data appear in the result.
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), ncol(df))
  expect_equal(colnames(result), expected = c("letters1", "letters2", "col3"))
  expect_equal(result[[1]], data[[1]])
  expect_equal(result[[2]], data[[2]])


  ## Identify columns by name
  cols <- "col2"
  data <- data.frame("letters" = letters[12:1])
  target <- patch_break(cols, data)

  # Note that the column names in the data appear in the result.
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), ncol(df))
  expect_equal(colnames(df), expected = c("col1", "col2", "col3"))
  expect_equal(colnames(result), expected = c("col1", "letters", "col3"))
  expect_equal(result[[1]], df[[1]])
  expect_equal(result[[2]], data[[1]])

  cols <- "col1"
  data <- data.frame("letters" = letters[1:12])
  target <- patch_break(cols, data)

  # Note that the column names in the data appear in the result.
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), ncol(df))
  expect_equal(colnames(result), expected = c("letters", "col2", "col3"))
  expect_equal(result[[1]], data[[1]])
  expect_equal(result[[2]], df[[2]])

  ## Test with multiple columns in the data.
  cols <- c("col1", "col2")
  data <- data.frame("letters1" = letters[1:12], "letters2" = letters[12:1])
  target <- patch_break(cols, data)

  # Note that the column names in the data appear in the result.
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), ncol(df))
  expect_equal(colnames(result), expected = c("letters1", "letters2", "col3"))
  expect_equal(result[[1]], data[[1]])
  expect_equal(result[[2]], data[[2]])


  ## Expect errors.
  cols <- 1:2
  data <- data.frame("letters1" = letters[1:12])
  # cols & data must have the same length.
  expect_error(patch_break(cols, data), regexp = "length")

  cols <- 1:4
  data <- data.frame("w" = 1:12, "x" = 1:12, "y" = 1:12, "z" = 1:12)

  target <- patch_break(cols, data)
  # cols argument must be compatible with the data frame.
  expect_error(target(df), "is_compatible_columns")

})
