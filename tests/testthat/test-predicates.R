require(testthat)
context("predicates")

test_that("the built-in predicates work", {

  # Valid vectors of column indices:
  params <- c(2L, 5L, 7L)
  expect_true(datadiff:::PREDICATE_COLUMNS(params))

  params <- c(2L, NULL, 7L)
  expect_true(datadiff:::PREDICATE_COLUMNS(params))

  # Invalid vectors of column indices:
  params <- c(0L, 5L, 7L)
  expect_false(datadiff:::PREDICATE_COLUMNS(params))

  params <- c(-2L, 5L, 7L)
  expect_false(datadiff:::PREDICATE_COLUMNS(params))

  params <- c(2L, NA, 7L)
  expect_false(datadiff:::PREDICATE_COLUMNS(params))

  params <- c(2L, NaN, 7L)
  expect_false(datadiff:::PREDICATE_COLUMNS(params))

  params <- c(2L, Inf, 7L)
  expect_false(datadiff:::PREDICATE_COLUMNS(params))

  params <- c(2, 5, 7)
  expect_false(datadiff:::PREDICATE_COLUMNS(params))

})
