require(testthat)
context("cycle function")

test_that("the cycle function works", {

  result <- cycle(1:5, cyc = 1:5)
  expect_equal(result, expected = c(2, 3, 4, 5, 1))

  result <- cycle(1:5, cyc = c(1, 4, 5))
  expect_equal(result, expected = c(4, 2, 3, 5, 1))

  result <- cycle(1:5, cyc = c(1, 4, 2, 5))
  expect_equal(result, expected = c(4, 5, 3, 2, 1))

  result <- cycle(1:5, cyc = c(2, 5))
  expect_equal(result, expected = c(1, 5, 3, 4, 2))
})
