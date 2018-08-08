require(testthat)
context("patch_recode")

test_that("the patch_recode constructor and get_patch_params function work", {

  encoding <- c("0" = FALSE, "1" = TRUE)

  ## Recode columns by index.
  target <- patch_recode(c(2L, 5L), encoding)

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_recode"))

  expected <- list("cols" = c(2L, 5L), "encoding" = encoding,
                   "one_to_one" = TRUE)
  expect_equal(get_patch_params(target), expected = expected)

  ## Recode columns by name.
  target <- patch_recode(c("mpg", "gear"), encoding)

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_recode"))

  expected <- list("cols" = c("mpg", "gear"), "encoding" = encoding,
                   "one_to_one" = TRUE)
  expect_equal(get_patch_params(target), expected = expected)
})

test_that("patch function application works", {

  encoding <- c("0" = FALSE, "1" = TRUE)
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))

  ## Recode by column index.
  target <- patch_recode(c(2L, 5L), encoding)
  expect_error(result <- target(df), regexp = "Invalid encoding")

  df[[2]] <- c(0, 0, 0, 2)
  target <- patch_recode(c(2L, 5L), encoding)
  expect_error(result <- target(df), regexp = "Invalid encoding")

  df[[2]] <- c(0, 0, 0, 1)
  df[[5]] <- c(1, 1, 0, 1)

  expect_false(is.logical(df[[2]]))
  expect_false(is.logical(df[[5]]))

  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(result[[1]], expected = df[[1]])
  expect_equal(result[[2]], expected = c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(result[[3]], expected = df[[3]])
  expect_equal(result[[4]], expected = df[[4]])
  expect_equal(result[[5]], expected = c(TRUE, TRUE, FALSE, TRUE))

  expect_true(is.logical(result[[2]]))
  expect_true(is.logical(result[[5]]))

  ## Test with NA values in the original data frame.
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))
  df[[2]] <- c(0, 0, 0, 1)
  df[[5]] <- c(1, NA, 0, 1)

  expect_false(is.logical(df[[2]]))
  expect_false(is.logical(df[[5]]))

  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(result[[1]], expected = df[[1]])
  expect_equal(result[[2]], expected = c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(result[[3]], expected = df[[3]])
  expect_equal(result[[4]], expected = df[[4]])
  expect_equal(result[[5]], expected = c(TRUE, NA, FALSE, TRUE))

  expect_true(is.logical(result[[2]]))
  expect_true(is.logical(result[[5]]))

  ## Test with categories of type character.
  df[[2]] <- letters[1:4]
  df[[5]] <- letters[4:1]

  expect_true(is.character(df[[2]]))
  expect_true(is.character(df[[5]]))

  encoding <- c("a"="A", "b"="B", "c"="C", "d"="D")
  target <- patch_recode(c(2L, 5L), encoding)

  result <- target(df)

  expect_true(is.data.frame(result))
  expect_true(is.character(result[[2]]))
  expect_true(is.character(result[[5]]))
  expect_equal(result[[2]], expected = toupper(letters[1:4]))
  expect_equal(result[[5]], expected = toupper(letters[4:1]))

  ## Test recoding from type character to type numeric.
  expect_true(is.character(df[[2]]))
  expect_true(is.character(df[[5]]))

  encoding <- c("a"=1, "b"=2, "c"=3, "d"=4)
  target <- patch_recode(c(2L, 5L), encoding)

  result <- target(df)

  expect_true(is.data.frame(result))
  expect_true(is.numeric(result[[2]]))
  expect_true(is.numeric(result[[5]]))
  expect_equal(result[[2]], expected = 1:4)
  expect_equal(result[[5]], expected = 4:1)

  ## Test recoding from type character to type integer
  expect_true(is.character(df[[2]]))
  expect_true(is.character(df[[5]]))

  encoding <- c("a"=1L, "b"=2L, "c"=3L, "d"=4L)
  target <- patch_recode(c(2L, 5L), encoding)

  result <- target(df)

  expect_true(is.data.frame(result))
  expect_true(is.integer(result[[2]]))
  expect_true(is.integer(result[[5]]))
  expect_equal(result[[2]], expected = 1:4)
  expect_equal(result[[5]], expected = 4:1)

  ## Test when a relevant column in the original data frame is a factor.
  df[[2]] <- as.factor(letters[1:4])
  df[[5]] <- letters[4:1]

  expect_true(is.factor(df[[2]]))
  expect_false(is.factor(df[[5]]))

  encoding <- c("a"="A", "b"="B", "c"="C", "d"="D")
  target <- patch_recode(c(2L, 5L), encoding)

  result <- target(df)

  expect_true(is.data.frame(result))
  # The recoded factor column is still a factor.
  expect_true(is.factor(result[[2]]))
  expect_false(is.factor(result[[5]]))
  expect_equal(result[[2]], expected = as.factor(toupper(letters[1:4])))
  expect_equal(result[[5]], expected = toupper(letters[4:1]))

  # Now make both relevant columns into factors.
  df[[5]] <- as.factor(letters[4:1])

  expect_true(is.factor(df[[2]]))
  expect_true(is.factor(df[[5]]))

  target <- patch_recode(c(2L, 5L), encoding)

  result <- target(df)

  expect_true(is.data.frame(result))
  # The recoded factor column is still a factor.
  expect_true(is.factor(result[[2]]))
  expect_true(is.factor(result[[5]]))
  expect_equal(result[[2]], expected = as.factor(toupper(letters[1:4])))
  expect_equal(result[[5]], expected = as.factor(toupper(letters[4:1])))


  #### Recode by column name.
  encoding <- c("0" = FALSE, "1" = TRUE)
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))
  names(df) <- letters[1:5]

  target <- patch_recode(c("b", "e"), encoding)
  expect_error(result <- target(df), regexp = "Invalid encoding")

  df[["b"]] <- c(0, 0, 0, 1)
  df[["e"]] <- c(1, 1, 0, 1)

  expect_false(is.logical(df[["b"]]))
  expect_false(is.logical(df[["e"]]))

  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(names(result), expected = letters[1:5])

  expect_equal(result[["a"]], expected = df[["a"]])
  expect_equal(result[["b"]], expected = c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(result[["c"]], expected = df[["c"]])
  expect_equal(result[["d"]], expected = df[["d"]])
  expect_equal(result[["e"]], expected = c(TRUE, TRUE, FALSE, TRUE))

  expect_true(is.logical(result[["b"]]))
  expect_true(is.logical(result[["e"]]))

  ## Test with NA values in the original data frame.
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))
  df[["b"]] <- c(0, 0, 0, 1)
  df[["e"]] <- c(1, NA, 0, 1)

  expect_false(is.logical(df[["b"]]))
  expect_false(is.logical(df[["e"]]))

  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(result[["a"]], expected = df[["a"]])
  expect_equal(result[["b"]], expected = c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(result[["c"]], expected = df[["c"]])
  expect_equal(result[["d"]], expected = df[["d"]])
  expect_equal(result[["e"]], expected = c(TRUE, NA, FALSE, TRUE))

  expect_true(is.logical(result[["b"]]))
  expect_true(is.logical(result[["e"]]))

  ## Test with categories of type character.
  df[["b"]] <- letters[1:4]
  df[["e"]] <- letters[4:1]

  expect_true(is.character(df[["b"]]))
  expect_true(is.character(df[["e"]]))

  encoding <- c("a"="A", "b"="B", "c"="C", "d"="D")
  target <- patch_recode(c("b", "e"), encoding)

  result <- target(df)

  expect_true(is.data.frame(result))
  expect_true(is.character(result[["b"]]))
  expect_true(is.character(result[["e"]]))
  expect_equal(result[["b"]], expected = toupper(letters[1:4]))
  expect_equal(result[["e"]], expected = toupper(letters[4:1]))

  ## Test when a relevant column in the original data frame is a factor.
  df[["b"]] <- as.factor(letters[1:4])
  df[["e"]] <- letters[4:1]

  expect_true(is.factor(df[["b"]]))
  expect_false(is.factor(df[["e"]]))

  target <- patch_recode(c("b", "e"), encoding)

  result <- target(df)

  expect_true(is.data.frame(result))
  # The recoded factor column is still a factor.
  expect_true(is.factor(result[["b"]]))
  expect_false(is.factor(result[["e"]]))
  expect_equal(result[["b"]], expected = as.factor(toupper(letters[1:4])))
  expect_equal(result[["e"]], expected = toupper(letters[4:1]))

  # Now make both relevant columns into factors.
  df[["e"]] <- as.factor(letters[4:1])

  expect_true(is.factor(df[["b"]]))
  expect_true(is.factor(df[["e"]]))

  target <- patch_recode(c("b", "e"), encoding)

  result <- target(df)

  expect_true(is.data.frame(result))
  # The recoded factor column is still a factor.
  expect_true(is.factor(result[["b"]]))
  expect_true(is.factor(result[["e"]]))
  expect_equal(result[["b"]], expected = as.factor(toupper(letters[1:4])))
  expect_equal(result[["e"]], expected = as.factor(toupper(letters[4:1])))

  # Apply to an incompatible data frame.
  target <- patch_recode(c(2L, 5L), encoding)
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))
  df[[2]] <- c(0, 0, 0, 1)
  expect_error(target(df[, 1:4]), regexp = "is_compatible_columns.*not TRUE")
})

test_that("the sample_patch_recode function works", {

  df <- mtcars

  condition <- function(x) { all(as.integer(x) == x) }
  codes <- letters[1:26]
  result <- sample_patch_recode(mtcars, codes = codes, condition = condition)

  expect_true(is_patch(result))
  expect_equal(patch_type(result), "recode")
  expect_equal(ncol(result(df)), ncol(df))
  expect_false(any(sapply(df, is.character)))
  expect_true(any(sapply(result(df), is.character)))

  # Test with too few codes.
  codes <- letters[1]
  expect_error(sample_patch_recode(mtcars, codes = codes, condition = condition),
               regexp = "cannot take a sample larger than the population")

  # Test the default codes argument (i.e. NULL); recodes to type character.
  result <- sample_patch_recode(mtcars, condition = condition)

  expect_true(is_patch(result))
  expect_equal(patch_type(result), "recode")
  expect_equal(ncol(result(df)), ncol(df))
  expect_false(any(sapply(df, is.character)))
  expect_true(any(sapply(result(df), is.character)))

  # Another test.
  condition <- function(x) { length(unique(x)) == 2 }
  codes <- c(TRUE, FALSE)
  result <- sample_patch_recode(mtcars, codes = codes, condition = condition)

  expect_true(is_patch(result))
  expect_equal(patch_type(result), "recode")
  expect_equal(ncol(result(df)), ncol(df))
  expect_false(any(sapply(df, is.logical)))
  expect_true(any(sapply(result(df), is.logical)))

})
