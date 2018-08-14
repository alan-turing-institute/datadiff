require(testthat)
context("patch_break")

test_that("the patch_break constructor and get_patch_params function work", {

  cols <- c(2L, 5L)
  data <- data.frame(1, 2)
  target <- patch_break(cols, data)

  expect_equal(patch_type(target), expected = "break")
  expect_true(is_patch(target, allow_composed = FALSE))

  cols <- c(4L)
  data <- data.frame(1)
  target <- patch_break(cols, data)

  expect_equal(patch_type(target), expected = "break")
  expect_true(is_patch(target, allow_composed = FALSE))
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

  # Test when the data column is given the same name as the original column.
  # This checks a bug observed where an erroneous suffix "1" was added to the
  # column name.
  cols <- 2L
  data <- data.frame(letters[12:1])
  colnames(data) <- colnames(df)[cols]
  target <- patch_break(cols, data)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), ncol(df))
  expect_equal(colnames(result), expected = c("col1", "col2", "col3"))
  expect_equal(result[[1]], df[[1]])
  expect_equal(result[[2]], data[[1]])
  expect_equal(result[[3]], df[[3]])

  # Test when colnames(data) is NULL.
  cols <- 2L
  data <- data.frame(letters[12:1])
  colnames(data) <- NULL
  target <- patch_break(cols, data)

  # Note that the original column names are preserved even in the broken column.
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), ncol(df))
  expect_equal(colnames(result), expected = c("col1", "col2", "col3"))
  expect_equal(result[[1]], df[[1]])
  expect_equal(result[[2]], data[[1]])
  expect_equal(result[[3]], df[[3]])

  # Another test.
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

test_that("the sample_patch_break function works", {

  df <- mtcars
  colname <- "BROKEN"

  result <- sample_patch_break(df, colname = colname)
  expect_true(is_patch(result))
  expect_equal(patch_type(result), "break")
  expect_equal(ncol(result(df)), ncol(df))
  expect_true(colname %in% colnames(result(df)))

  # Test breaking a second column.
  df <- result(df)
  result <- sample_patch_break(df, colname = colname)
  expect_true(is_patch(result))
  expect_equal(patch_type(result), "break")
  expect_equal(ncol(result(df)), ncol(df))
  expect_equal(sum(colnames(result(df)) == colname), expected = 2)

  # Test with no colname argument (i.e. preserve all colnames).
  df <- mtcars
  result <- sample_patch_break(df)
  expect_true(is_patch(result))
  expect_equal(patch_type(result), "break")
  expect_equal(ncol(result(df)), ncol(df))
  expect_true(setequal(colnames(result(df)), c(colnames(df))))

  # Test with parameters passed to rdist.
  df <- mtcars
  set.seed(147)
  result <- sample_patch_break(df, mean = 10, sd = 4, colname = colname)
  expect_true(is_patch(result))
  expect_equal(patch_type(result), "break")
  expect_equal(mean(get_patch_params(result)[["data"]][[colname]]), 10,
               tolerance = 0.16)

  # Test with a discrete distribution for the data.
  rdist <- function(n, ...) { sample(letters[1:3], size = n, ...) }
  expect_error(sample_patch_break(df, rdist = rdist),
               regexp = "cannot take a sample larger than the population")

  result <- sample_patch_break(df, rdist = rdist, colname = colname, replace = TRUE)
  expect_true(is_patch(result))
  expect_equal(patch_type(result), "break")
  expect_true(setequal(unique(get_patch_params(result)[["data"]][[colname]]),
                       letters[1:3]))

})
