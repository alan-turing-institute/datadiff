require(testthat)
context("ks function")

test_that("the ks function works", {

  set.seed(22)

  ## Test with discrete numeric vectors.
  x <- sample(1:10, size = 100, replace = TRUE)

  expect_identical(ks(x, x), 0)

  # The order of the elements in the vector is unimportant.
  expect_identical(ks(x, x[sample.int(length(x))]), 0)

  y <- sample(1:10, size = 100, replace = TRUE)

  result <- ks(x, y)
  expect_true(is.numeric(result))
  expect_true(length(result) == 1)
  expect_true(result > 0 && result < 1)

  # The order of the elements in the vector is unimportant.
  expect_identical(ks(x, y[sample.int(length(y))]), result)
  # The order of the arguments is unimportant.
  expect_identical(ks(y, x), result)

  # Test when the ranges of x & y are disjoint.
  y <- sample(11:20, size = 100, replace = TRUE)

  expect_identical(ks(x,y), 1.0)

  ## Test with continuous numeric vectors.
  x <- rnorm(100)

  expect_identical(ks(x, x), 0)
  expect_identical(ks(x, x[sample.int(length(x))]), 0)

  y <- rnorm(100)

  result <- ks(x, y)
  expect_true(is.numeric(result))
  expect_true(length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(ks(x, y), ks(y, x))

  y <- rnorm(200)

  result <- ks(x, y)
  expect_true(is.numeric(result))
  expect_true(length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(ks(x, y), ks(y, x))

  ## Test with a mixture of discrete and continuous numeric vectors.
  x <- rnorm(100)
  y <- sample(1:10, size = 100, replace = TRUE)

  result <- ks(x, y)

  expect_true(is.numeric(result))
  expect_true(length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(ks(x, y), ks(y, x))

  ## Arguments must be numeric (else ecdf would throw a confusing error).
  x <- sample(1:10, size = 100, replace = TRUE)
  y <- sample(1:10, size = 100, replace = TRUE)

  expect_error(ks(letters[x], letters[y]),
               regexp = "must be numeric vectors or ordered factors")
  expect_error(ks(factor(x), factor(y)),
               regexp = "must be numeric vectors or ordered factors")

  ## Test in the presence of NA values.
  x <- rnorm(100)
  y <- x
  x[2] <- NA

  expect_identical(ks(x, x), 0)

  result <- ks(x, y)
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  # NA values are ignored.
  expect_identical(ks(x[!is.na(x)], y), result)

  x[-3] <- NA

  expect_identical(ks(x, x), 0)

  result <- ks(x, y)
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  # NA values are ignored.
  expect_identical(ks(x[!is.na(x)], y), result)

  # If _all_ values are NA an error is thrown (by ecdf).
  x[] <- NA

  expect_error(ks(x, x),
               regexp = "must have 1 or more non-missing values")
  expect_error(ks(x, y),
               regexp = "must have 1 or more non-missing values")
  expect_error(ks(y, x),
               regexp = "must have 1 or more non-missing values")

  ## Test with ordered categorical data.
  u <- letters[rep(1:10, 10)[sample.int(100, 100)]] # Uniform masses on a:j
  x <- letters[rep(seq(1, 9, by = 2), 20)[sample.int(100, 100)]]
  y <- letters[rep(1:5, 20)[sample.int(100, 100)]]

  expect_error(ks(x, u),
               regexp = "Arguments must be numeric vectors or ordered factors")
  expect_error(ks(factor(x), factor(u)),
               regexp = "Arguments must be numeric vectors or ordered factors")
  result <- ks(factor(x, ordered = TRUE), factor(u, ordered = TRUE))

  # The K-S "distance" between u & x is less than that between u & y, since x
  # is more uniform. To see this compare:
  # plot(ecdf(factor(u)))
  # plot(ecdf(factor(x, levels = letters[1:10])))
  # plot(ecdf(factor(y)), xlim=c(0, 10))
  expect_true(result < ks(factor(y, ordered = TRUE), factor(u, ordered = TRUE)))

  # Test with ordered factors whose orders conflict.
  f1 <- factor(u, ordered = TRUE)
  lev <- levels(f1)[sample.int(10, 10)] # Jumble the ordering.
  f2 <- factor(u, levels = lev, ordered = TRUE)

  expect_error(ks(f1, f2), "levels must determine an ordering")

  # Check that integer data may be treated as ordered categorical data or
  # numerical data, with equal results.
  u <- rep(1:10, 10)[sample.int(100, 100)]
  x <- rep(seq(1, 9, by = 2), 20)[sample.int(100, 100)]
  y <- rep(1:5, 20)[sample.int(100, 100)]

  expect_identical(ks(factor(x, ordered = TRUE), factor(u, ordered = TRUE)),
                   ks(x, u))
  expect_identical(ks(factor(y, ordered = TRUE), factor(u, ordered = TRUE)),
                   ks(y, u))
  expect_identical(ks(factor(x, ordered = TRUE), factor(y, ordered = TRUE)),
                   ks(x, y))

  # Test with ordered factors whose orders don't overlap sufficiently
  # (hence don't determine an ordering).
  f1 <- factor(x, ordered = TRUE)
  z <- letters[rep(c(1:5, 8), 20)[sample.int(100, 100)]]
  f2 <- factor(z, ordered = TRUE)

  expect_error(ks(f1, f2), "levels must determine an ordering")
})
