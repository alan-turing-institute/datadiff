require(testthat)
context("patch_identity")

test_that("the patch_identity constructor function works", {

  target <- patch_identity()

  expect_true(is_patch(target))
  expect_true(is_identity_patch(target))
})

test_that("the print_patch_params method works", {

  target <- patch_identity()
  expect_identical(print_patch_params(target), character(1))
})
