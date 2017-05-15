require(testthat)
context("patch")

test_that("the is_patch function works", {

  p1 <- patch_identity()
  expect_true(is_patch(p1))

  p2 <- patch_identity()

  p <- purrr::compose(p2, p1)
  expect_false(is_patch(p, allow_composed = FALSE))
  expect_true(is_patch(p, allow_composed = TRUE))

  p3 <- patch_identity()
  p2 <- purrr::compose(p3, p2)
  p <- purrr::compose(p2, p1)

  expect_false(is_patch(p, allow_composed = FALSE))
  expect_true(is_patch(p, allow_composed = TRUE))
})

test_that("the is_identity_patch function works", {

  p1 <- patch_identity()
  expect_true(is_identity_patch(p1))
  expect_true(is_identity_patch(p1, allow_composed = FALSE))

  p2 <- patch_identity()
  expect_true(is_identity_patch(p2))
  expect_true(is_identity_patch(p2, allow_composed = FALSE))

  expect_true(is_identity_patch(compose_patch(p1, p2), allow_composed = TRUE))
  expect_false(is_identity_patch(compose_patch(p1, p2), allow_composed = FALSE))

  expect_true(is_identity_patch(purrr::compose(p1, p2), allow_composed = TRUE))
  expect_false(is_identity_patch(purrr::compose(p1, p2), allow_composed = FALSE))

  expect_true(is_identity_patch(compose_patch(p1, p2, p1), allow_composed = TRUE))
  expect_false(is_identity_patch(compose_patch(p1, p2, p1), allow_composed = FALSE))

  p3 <- patch_shift(1L, shift = 1)
  expect_false(is_identity_patch(compose_patch(p3, p1, p2), allow_composed = TRUE))
  expect_false(is_identity_patch(compose_patch(p1, p3, p2), allow_composed = TRUE))
  expect_false(is_identity_patch(compose_patch(p1, p2, p3), allow_composed = TRUE))

})

test_that("the patch_type function works", {

  p <- patch_identity()
  expect_identical(patch_type(p), "identity")
  expect_identical(patch_type(p, short = FALSE), "patch_identity")

  p1 <- patch_shift(1L, shift = 1)
  expect_identical(patch_type(p1), "shift")

  p2 <- patch_scale(1L, scale_factor = 2)
  expect_identical(patch_type(p2), "scale")

  p <- purrr::compose(p2, p1)
  expect_identical(patch_type(p), c("shift", "scale"))
})


test_that("the get_patch_params function works", {

  p1 <- patch_shift(1L, shift = 1)

  result <- get_patch_params(p1)
  expect_true(is.list(result))
  expect_identical(result, expected = list("cols"=1L, "shift"=1))

  p2 <- patch_scale(1L, scale_factor = 2)
  p <- purrr::compose(p2, p1)

  expect_false(is_patch(p, allow_composed = FALSE))
  expect_true(is_patch(p, allow_composed = TRUE))

  result <- get_patch_params(p)
  expect_true(is.list(result))
  expect_equal(length(result), 2)

  expect_true(is.list(result[[1]]))
  expect_identical(result[[1]], expected = list("cols"=1L, "shift"=1))

  expect_true(is.list(result[[2]]))
  expect_identical(result[[2]], expected = list("cols"=1L, "scale_factor"=2))
})

test_that("the decompose_patch function works", {

  p1 <- patch_shift(1L, shift = 1)
  p2 <- patch_scale(1L, scale_factor = 2)

  expect_equal(decompose_patch(p1), p1)
  expect_equal(decompose_patch(p2), p2)

  p <- purrr::compose(p2, p1)

  result <- decompose_patch(p)
  expect_equal(result, list(p1, p2))

  # Test the tree recursion.
  result <- decompose_patch(purrr::compose(p1, p))
  expect_equal(result, list(p1, p2, p1))

  result <- decompose_patch(purrr::compose(p, p1))
  expect_equal(result, list(p1, p1, p2))

})

test_that("the simplify_patch function works", {

  p <- patch_identity()
  p1 <- patch_shift(1L, shift = 1)
  p2 <- patch_scale(1L, scale_factor = 3)

  target <- compose_patch(p, p)
  expect_equal(simplify_patch(target), expected = p)

  target <- compose_patch(p, p, p1)
  expect_equal(decompose_patch(simplify_patch(target)),
               expected = p1)

  target <- compose_patch(p, compose_patch(p1, p2), p)
  expected <- decompose_patch(compose_patch(p1, p2))
  expect_equal(decompose_patch(simplify_patch(target)), expected = expected)

  target <- compose_patch(p, compose_patch(p1, p2), p)
  expected <- decompose_patch(compose_patch(p1, p2))
  expect_equal(decompose_patch(simplify_patch(target)), expected = expected)

  target <- compose_patch(p1, p, compose_patch(p, p2), p, compose_patch(p, p1))
  expected <- decompose_patch(compose_patch(p1, p2, p1))
  expect_equal(decompose_patch(simplify_patch(target)), expected = expected)
})

test_that("the apply_patch function works", {

  p1 <- patch_shift(1L, shift = 1)
  p2 <- patch_scale(1L, scale_factor = 3)

  df <- data.frame(x = rep(1, 10), y = rep(1, 10))

  expected <- data.frame(x = rep(2, 10), y = rep(1, 10))
  expect_identical(apply_patch(df, p1), expected)
  expect_identical(apply_patch(df, p1), p1(df))

  expected <- data.frame(x = rep(3, 10), y = rep(1, 10))
  expect_identical(apply_patch(df, p2), expected)
  expect_identical(apply_patch(df, p2), p2(df))

  expected <- data.frame(x = rep(6, 10), y = rep(1, 10))
  expect_identical(apply_patch(apply_patch(df, p1), p2), expected)
  expect_identical(apply_patch(apply_patch(df, p1), p2), p2(p1(df)))

  expected <- data.frame(x = rep(4, 10), y = rep(1, 10))
  expect_identical(apply_patch(apply_patch(df, p2), p1), expected)
  expect_identical(apply_patch(apply_patch(df, p2), p1), p1(p2(df)))

  # Note that the magrittr pipe operator %>% also works:
  # library(magrittr)
  # expected <- data.frame(x = rep(6, 10), y = rep(1, 10))
  # expect_identical(df %>% apply_patch(p1) %>% apply_patch(p2), expected)

})
