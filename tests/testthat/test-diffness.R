require(testthat)
context("diffness function")

test_that("the diffness function works", {

  # TODO. Test with all data types _including_ mixed types.
  set.seed(22)

  ## Test with categorical vectors.
  x <- sample(letters[1:5], size = 100, replace = TRUE)

  expect_true(is.character(x))
  expect_identical(diffness(x, x), 0)
  expect_identical(diffness(x, x[sample.int(length(x))]), 0)

  y <- sample(letters[1:5], size = 100, replace = TRUE)

  result <- diffness(x, y)
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(diffness(x, y[sample.int(length(y))]), diffness(x, y))
  expect_identical(diffness(x, y), diffness(y, x))

  y <- sample(letters[1:3], size = 100, replace = TRUE)

  result <- diffness(x, y)
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(diffness(x, y[sample.int(length(y))]), diffness(x, y))
  expect_identical(diffness(x, y), diffness(y, x))

  y <- sample(letters[1:5], size = 200, replace = TRUE)

  result <- diffness(x, y)
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(diffness(x, y[sample.int(length(y))]), diffness(x, y))
  expect_identical(diffness(x, y), diffness(y, x))

  ## Test with logical vectors.
  set.seed(22)

  x <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)

  expect_true(is.logical(x))
  expect_identical(diffness(x, x), 0)
  expect_identical(diffness(x, x[sample.int(length(x))]), 0)

  y <- sample(c(TRUE, FALSE), size = 100, replace = TRUE)

  result <- diffness(x, y)
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(diffness(x, y[sample.int(length(y))]), diffness(x, y))
  expect_identical(diffness(x, y), diffness(y, x))

  y <- sample(c(TRUE, FALSE), size = 200, replace = TRUE)

  result <- diffness(x, y)
  expect_true(is.numeric(result) && length(result) == 1)
  # NOTE: if x and y were to, by chance, have equal proportions of Ts and Fs
  # then we would have diffness(x, y) = 0.
  expect_true(result > 0 && result < 1)
  expect_identical(diffness(x, y[sample.int(length(y))]), diffness(x, y))
  expect_identical(diffness(x, y), diffness(y, x))

  ## Test with continuous numeric vectors.
  x <- rnorm(100)

  expect_identical(diffness(x, x), 0)
  expect_identical(diffness(x, x[sample.int(length(x))]), 0)

  y <- rnorm(100)

  result <- diffness(x, y)
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(diffness(x, y[sample.int(length(y))]), diffness(x, y))
  expect_identical(diffness(x, y), diffness(y, x))

  y <- rnorm(200)

  result <- diffness(x, y)
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(diffness(x, y[sample.int(length(y))]), diffness(x, y))
  expect_identical(diffness(x, y), diffness(y, x))

  # Test with mixtures of categorical and continuous vectors.
  x <- sample(1:10, size = 100, replace = TRUE)
  y <- sample(letters[1:5], size = 100, replace = TRUE)
  z <- rnorm(100)

  expect_identical(diffness(x, y), 1.0)
  expect_identical(diffness(x, z), 1.0)
  expect_identical(diffness(y, x), 1.0)
  expect_identical(diffness(y, z), 1.0)
  expect_identical(diffness(z, x), 1.0)
  expect_identical(diffness(z, y), 1.0)

  ## Test with integer vectors.
  ## By default, integer vectors are treated as ordered categorical data.

  x <- sample(1:10, size = 100, replace = TRUE)

  expect_true(is.integer(x))
  expect_identical(diffness(x, x), 0)
  expect_identical(diffness(x, x[sample.int(length(x))]), 0)

  y <- sample(1:10, size = 100, replace = TRUE)

  result <- diffness(x, y)
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(diffness(x, y), diffness(y, x))

  y <- sample(1:5, size = 100, replace = TRUE)

  result <- diffness(x, y)
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(diffness(x, y), diffness(y, x))

  y <- sample(1:10, size = 200, replace = TRUE)

  result <- diffness(x, y)
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0 && result < 1)
  expect_identical(diffness(x, y), diffness(y, x))

  ## Test the difference between ordered and un-ordered categories.

  u <- rep(1:10, 10)[sample.int(100, 100)] # Uniform masses on 1:10
  x <- rep(seq(from = 1, to = 9, by = 2), 20)[sample.int(100, 100)]
  y <- rep(1:5, 20)[sample.int(100, 100)]

  # Taking into account the ordering of the integers, the mismatch between u
  # and x is less than that between u and y, since x is more uniformly
  # distributed. In contrast, if we use the total variation distance (i.e. we
  # ignore the ordering) the mismatches are identical.
  fu <- factor(u, ordered = TRUE)
  fx <- factor(x, ordered = TRUE)
  fy <- factor(y, ordered = TRUE)
  expect_true(diffness(fx, fu) < diffness(fy, fu))
  expect_identical(diffness(fx, fu, diff = tv), diffness(fy, fu, diff = tv))

  ## Test with data frames.

  x <- as.data.frame(matrix(rnorm(500), nrow = 100, ncol = 5))
  y <- as.data.frame(matrix(rnorm(500), nrow = 100, ncol = 5))

  result <- diffness(x, y)
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0)
  expect_identical(diffness(x, y), diffness(y, x))

  # Test with data frames containing data of different types.
  v <- sample(letters[1:5], size = 100, replace = TRUE)
  y[[5]] <- v

  result <- diffness(x, y)
  expect_true(is.numeric(result) && length(result) == 1)
  expect_true(result > 0)
  expect_identical(diffness(x, y), diffness(y, x))

  expect_identical(diffness(x, y), diffness(x[1:4], y[1:4]) + 1)

  # Test with data frames having different numbers of columns.
  expect_identical(diffness(x, y[1:3]), diffness(x[1:3], y[1:3]) + 2)

  ## Test with mixtures of vectors and data frames.
  v <- rnorm(100)

  expect_error(diffness(x, v), regexp = "is.data.frame")
  expect_error(diffness(v, x), regexp = "is.vector")

  v <- sample(1:10, size = 100, replace = TRUE)

  expect_error(diffness(x, v), regexp = "is.data.frame")
  expect_error(diffness(v, x), regexp = "is.vector")

  v <- sample(letters[1:5], size = 100, replace = TRUE)

  expect_error(diffness(x, v), regexp = "is.data.frame")
  expect_error(diffness(v, x), regexp = "is.vector")

  # Test with one or more empty data frames.
  y <- data.frame()
  expect_error(diffness(x, y), regexp = "length")
  expect_error(diffness(y, x), regexp = "length")
  expect_error(diffness(y, y), regexp = "length")
})

