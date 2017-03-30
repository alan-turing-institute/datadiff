require(testthat)
context("tv function")

test_that("the tv function works", {

  set.seed(22)

  x <- sample(1:10, size = 100, replace = TRUE)

  expect_identical(tv(factor(x), factor(x)), 0)

  # The order of the elements in the vector is unimportant.
  expect_identical(tv(factor(x), factor(x[sample.int(length(x))])), 0)

  y <- sample(1:10, size = 100, replace = TRUE)

  expect_error(tv(x, y), regexp = "is.factor") # Arguments must be factors.

  result <- tv(factor(x), factor(y))
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)

  # The order of the elements in the vector is unimportant.
  expect_identical(tv(factor(x), factor(y[sample.int(length(y))])), result)
  # The order of the arguments is unimportant.
  expect_identical(tv(factor(y), factor(x)), result)

  # Test when x & y have different levels.
  y <- sample(1:5, size = 100, replace = TRUE)

  result <- tv(factor(x), factor(y))
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)

  # The order of the elements in the vector is unimportant.
  expect_identical(tv(factor(x), factor(y[sample.int(length(y))])), result)
  # The order of the arguments is unimportant.
  expect_identical(tv(factor(y), factor(x)), result)

  ## Test when x & y have different levels.
  x <- sample(1:5, size = 100, replace = TRUE)
  y <- sample(1:3, size = 100, replace = TRUE)
  a <- letters[x]
  b <- letters[y]

  result <- tv(factor(a), factor(b))
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(result, tv(factor(x), factor(y)))

  # The order of the elements in the vector is unimportant.
  expect_identical(tv(factor(a), factor(b[sample.int(length(b))])), result)

  ## Test when x & y have disjoint levels.
  y <- sample(11:20, size = 100, replace = TRUE)

  expect_identical(tv(factor(x), factor(y)), 1.0)

  # The encoding of categories is unimportant.
  x <- sample(1:10, size = 100, replace = TRUE)
  y <- sample(1:10, size = 100, replace = TRUE)
  a <- letters[x]
  b <- letters[y]

  result <- tv(factor(a), factor(b))
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(result, tv(factor(x), factor(y)))

  # The order of the elements in the vector is unimportant.
  expect_identical(tv(factor(a), factor(b[sample.int(length(b))])), result)

  ## Test with logical vectors.
  x <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)

  expect_true(is.logical(x))
  expect_identical(tv(factor(x), factor(x)), 0)
  expect_identical(tv(factor(x), factor(x[sample.int(length(x))])), 0)

  y <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)

  result <- tv(factor(x), factor(y))
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(tv(factor(x), factor(y[sample.int(length(y))])), result)
  expect_identical(tv(factor(y), factor(x)), result)

  y <- sample(c(TRUE, FALSE), size = 200, replace = TRUE)

  result <- tv(factor(x), factor(y))
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(tv(factor(x), factor(y[sample.int(length(y))])), result)
  expect_identical(tv(factor(y), factor(x)), result)

  ## Test when one or both arguments are empty.
  x <- integer()
  expect_error(tv(factor(x), factor(x)),
               regexp = "must have one or more non-missing values")

  y <- sample(1:10, size = 100, replace = TRUE)
  expect_error(tv(factor(x), factor(y)),
               regexp = "must have one or more non-missing values")

  ## Test in the presence of NA values.
  x <- sample(1:10, size = 100, replace = TRUE)
  y <- x
  x[2] <- NA

  expect_identical(tv(factor(x), factor(x)), 0)

  result <- tv(factor(x), factor(y))
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  # NA values are ignored.
  expect_identical(tv(factor(x[!is.na(x)]), factor(y)), result)

  x[-3] <- NA

  expect_identical(tv(factor(x), factor(x)), 0)

  result <- tv(factor(x), factor(y))
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  # NA values are ignored.
  expect_identical(tv(factor(x[!is.na(x)]), factor(y)), result)

  # If _all_ values are NA an error is thrown.
  x[] <- NA

  expect_error(tv(factor(x), factor(x)),
               regexp = "must have one or more non-missing values")
  expect_error(tv(factor(x), factor(y)),
               regexp = "must have one or more non-missing values")
  expect_error(tv(factor(y), factor(x)),
               regexp = "must have one or more non-missing values")

})
