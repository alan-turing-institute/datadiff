require(testthat)
context("encode.patch methods")

test_that("the encode_patch_params function works", {

  column_indices <- c(2L, 5L, 7L)
  encode_factor <- c("0" = FALSE, "1" = TRUE)
  result <- encode_patch_params(column_indices, encode_factor)

  expect_true(is.list(result))
  expect_equal(length(result), expected = 2)
  expect_true(setequal(names(result), c(datadiff:::COLUMNS, datadiff:::ENCODING)))
  expect_equal(result[[datadiff:::COLUMNS]], expected = column_indices)
  # The encode_factor is converted to a factor.
  expect_equal(result[[datadiff:::ENCODING]], expected = as.factor(encode_factor))
  # Note that we must use utils::type.convert to recover the factor values/levels in their original type.
  expect_true(is.logical(utils::type.convert(levels(result[[datadiff:::ENCODING]]))))

  column_indices <- c(2L, -5L, 7L)
  expect_error(encode_patch_params(column_indices, encode_factor), regexp = "Invalid column_indices")

  column_indices <- c(2L, 5L, 7L)
  encode_factor <- c("0" = FALSE, "0" = TRUE)
  expect_error(encode_patch_params(column_indices, encode_factor), regexp = "Invalid encode_factor")
  encode_factor <- c("0" = FALSE, "1" = FALSE)
  expect_error(encode_patch_params(column_indices, encode_factor), regexp = "Invalid encode_factor")
  encode_factor <- c("0" = FALSE, "1" = NA)
  expect_error(encode_patch_params(column_indices, encode_factor), regexp = "Invalid encode_factor")
  encode_factor <- character()
  expect_error(encode_patch_params(column_indices, encode_factor), regexp = "Invalid encode_factor")
})

test_that("the patch constructor and patch_type methods work", {

  ## Encode patch.
  params <- encode_patch_params(c(2L, 5L, 7L), c("0" = FALSE, "1" = TRUE))
  target <- patch(params)

  expect_true(is(target, "patch"))
  expect_true(is(target, datadiff:::TYPE_ENCODE))
  expect_equal(patch_type(target), expected = datadiff:::TYPE_ENCODE)
  expect_equal(patch_params(target), expected = params)
})

test_that("the is_compatible method works", {

  ## Encode patch.
  params <- encode_patch_params(c(2L), c("0" = FALSE, "1" = TRUE))
  target <- patch(params)

  df <- as.data.frame(matrix(1:28, nrow = 4, ncol = 7))
  expect_false(is_compatible(target, df))

  df[[2]] <- c(0, 0, 0, 2)
  expect_false(is_compatible(target, df))

  # Test the 'strict' argument.
  expect_true(is_compatible(target, df, strict = FALSE))

  df[[2]] <- c(0, 0, 0, 1)
  expect_true(is_compatible(target, df))

  # Test with multiple column indices.
  params <- encode_patch_params(c(2L, 5L), c("0" = FALSE, "1" = TRUE))
  target <- patch(params)
  expect_false(is_compatible(target, df))

  df[[5]] <- c(1, 2, 0, 1)
  expect_false(is_compatible(target, df))

  # Test the 'strict' argument.
  expect_true(is_compatible(target, df, strict = FALSE))

  df[[5]] <- c(1, 1, 0, 1)
  expect_true(is_compatible(target, df))

  df <- df[, 1:4]
  expect_false(is_compatible(target, df))

  params <- encode_patch_params(c(2L), c("0" = FALSE, "1" = TRUE))
  target <- patch(params)
  expect_true(is_compatible(target, df))
})

test_that("patch function application works", {

  ## Encode patch.
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))
  df[[2]] <- c(0, 0, 0, 1)
  df[[5]] <- c(1, 1, 0, 1)

  expect_false(is.logical(df[[2]]))
  expect_false(is.logical(df[[5]]))

  params <- encode_patch_params(c(2L, 5L), c("0" = FALSE, "1" = TRUE))
  target <- patch(params)
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

  # Apply to an incompatible data frame.
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))
  df[[2]] <- c(0, 0, 0, 1)

  params <- encode_patch_params(c(2L, 5L), c("0" = FALSE, "1" = TRUE))
  target <- patch(params)

  # Incompatible due to non-binary values in column 5.
  expect_error(target(df), regexp = "Incompatible")

  # Incompatible due to column index out of bounds.
  expect_error(target(df[, 1:4]), regexp = "Incompatible")

  ## Test the 'strict' argument.
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))
  df[[2]] <- c(0, 0, 0, 1)

  expect_false(is.logical(df[[2]]))

  params <- encode_patch_params(c(2L), c("0" = FALSE))
  target <- patch(params)

  # With strict = TRUE (the default) the patch is incompatible.
  expect_error(target(df), regexp = "Incompatible")

  # With strict = FALSE the patch is compatible but NA values are introduced.
  expect_warning(result <- target(df, strict=FALSE), regexp = "NAs introduced due to non-strict encoding.")

  expect_true(is.data.frame(result))
  expect_true(is.logical(result[[2]]))
  # Only the fourth element in column 2 is NA, since the encoding handled '0's correctly but not '1's.
  expect_false(any(is.na(result[[2]][1:3])))
  expect_true(is.na(result[[2]][4]))

  ## Test the convert_type argument.
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))
  df[[2]] <- c(0, 0, 0, 1)
  df[[5]] <- c(1, 1, 0, 1)

  expect_false(is.character(df[[2]]))
  expect_false(is.character(df[[5]]))

  params <- encode_patch_params(c(2L, 5L), c("0" = FALSE, "1" = TRUE))
  target <- patch(params)
  result <- target(df, convert_type = FALSE)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), expected = ncol(df))
  expect_equal(result[[1]], expected = df[[1]])
  # With convert_type = FALSE, all re-encoded columns have type character.
  expect_equal(result[[2]], expected = c("FALSE", "FALSE", "FALSE", "TRUE"))
  expect_equal(result[[3]], expected = df[[3]])
  expect_equal(result[[4]], expected = df[[4]])
  expect_equal(result[[5]], expected = c("TRUE", "TRUE", "FALSE", "TRUE"))

  expect_true(is.character(result[[2]]))
  expect_true(is.character(result[[5]]))

  ## Test with NA values in the original data frame.
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))
  df[[2]] <- c(0, 0, 0, NA)

  params <- encode_patch_params(c(2L), c("0" = FALSE))
  target <- patch(params)

  # Now there is no warning, because the fourth element in column 2 was already NA.
  result <- target(df, strict=FALSE)

  expect_true(is.data.frame(result))
  expect_true(is.logical(result[[2]]))
  expect_false(any(is.na(result[[2]][1:3])))
  expect_true(is.na(result[[2]][4]))

  df[[2]] <- c(0, NA, NA, NA)

  # This time there is no warning as the fourth element in column 2 was already NA.
  result <- target(df, strict=FALSE)

  expect_true(is.data.frame(result))
  expect_true(is.logical(result[[2]]))
  expect_false(is.na(result[[2]][1]))
  expect_true(all(is.na(result[[2]][2:4])))

  ## Test with categories of type character.
  df[[2]] <- letters[1:4]

  expect_true(is.character(df[[2]]))

  params <- encode_patch_params(c(2L), c("a"="A", "b"="B", "c"="C", "d"="D"))
  target <- patch(params)

  result <- target(df)

  expect_true(is.data.frame(result))
  expect_true(is.character(result[[2]]))
  expect_false(any(is.na(result[[2]])))
  expect_equal(result[[2]], expected = toupper(letters[1:4]))

  ## Test when a relevant column in the original data frame is itself a factor.
  df[[2]] <- as.factor(letters[1:4])

  expect_true(is.factor(df[[2]]))

  params <- encode_patch_params(c(2L), c("a"="A", "b"="B", "c"="C", "d"="D"))
  target <- patch(params)

  result <- target(df)

  expect_true(is.data.frame(result))
  # The re-encoded factor column is still a factor.
  expect_true(is.factor(result[[2]]))
  expect_false(any(is.na(result[[2]])))
  expect_equal(result[[2]], expected = as.factor(toupper(letters[1:4])))

  # Test the convert_type argument (for the case when a relevant column in the original data frame is itself a factor.)
  df <- as.data.frame(matrix(1:20, nrow = 4, ncol = 5))
  df[[2]] <- as.factor(letters[1:4])

  expect_true(is.factor(df[[2]]))

  params <- encode_patch_params(c(2L), c("a"="A", "b"="B", "c"="C", "d"="D"))
  target <- patch(params)

  result <- target(df, convert_type = FALSE)

  expect_true(is.factor(result[[2]]))
})
