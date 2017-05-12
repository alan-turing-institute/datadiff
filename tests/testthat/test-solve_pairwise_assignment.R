require(testthat)
context("solve_pairwise_assignment function")

test_that("the solve_pairwise_assignment function works", {

  ## Test with a square matrix.
  m <- matrix(1, nrow = 5, ncol = 5) - diag(5)
  expect_identical(solve_pairwise_assignment(m), expected = 1:5)

  # Note the use of order to convert from the solution to permutation notation.
  perm <- as.integer(c(2, 1, 3, 4, 5))
  expect_identical(order(solve_pairwise_assignment(m[, perm])), expected = perm)

  perm <- as.integer(c(4, 1, 2, 5, 3))
  expect_identical(order(solve_pairwise_assignment(m[, perm])), expected = perm)

  ## Test with nrow(m) < ncol(m).
  expect_identical(solve_pairwise_assignment(m[1:3, ]), expected = 1:5)

  perm <- as.integer(c(2, 1, 3, 4, 5))
  expect_identical(order(solve_pairwise_assignment(m[1:3, perm])), expected = perm)

  perm <- as.integer(c(4, 1, 5, 2, 3))
  expect_identical(order(solve_pairwise_assignment(m[1:3, perm])), expected = perm)

  ## Test with ncol(m) < nrow(m).
  expect_identical(solve_pairwise_assignment(m[, 1:3]), expected = 1:5)

  perm <- as.integer(c(2, 1, 3, 4, 5))
  expect_identical(order(solve_pairwise_assignment(m[, perm][, 1:3])), expected = perm)

  perm <- as.integer(c(4, 1, 5, 2, 3))
  expect_identical(order(solve_pairwise_assignment(m[, perm][, 1:3])), expected = perm)
})
