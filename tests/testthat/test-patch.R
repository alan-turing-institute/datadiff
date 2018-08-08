require(testthat)
context("patch")

test_that("the is_patch function works", {

  p1 <- patch_identity()
  expect_true(is_patch(p1, allow_composed = TRUE))
  expect_true(is_patch(p1, allow_composed = FALSE))

  p2 <- patch_identity()

  # Test when composing with the purrr::compose function.
  p <- purrr::compose(p2, p1)
  expect_false(is_patch(p, allow_composed = FALSE))
  expect_true(is_patch(p, allow_composed = TRUE))

  # Test when composing with compose_patch.
  p <- compose_patch(p2, p1)
  expect_false(is_patch(p, allow_composed = FALSE))
  expect_true(is_patch(p, allow_composed = TRUE))

  # Test with a function which is not a patch.
  expect_false(is_patch(toupper))

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

  p <- purrr::compose(p1, p1)
  expect_identical(patch_type(p), c("shift", "shift"))

  p <- purrr::compose(p1, p1)
  expect_identical(patch_type(p, unique = TRUE), c("shift"))
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

  expect_equal(decompose_patch(p1), list(p1))
  expect_equal(decompose_patch(p2), list(p2))

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
               expected = list(p1))

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

test_that("the admit_columns function works", {

  ####
  #### Test with a column-wise patch.
  ####

  cols_param_name <- "cols"

  ## Shift patch on column 1L.

  # Admitting a column at position 1L shifts the column indices in the result
  # up by one.
  p <- patch_shift(1L, shift = 1)
  expect_identical(get_patch_params(p)[[cols_param_name]], 1L)

  admit_columns(p, 1L, cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]], 2L)

  # Admitting a column at position 2L (or more) has no effect on the column
  # indices in the result (since the original column index was less than 2).
  p <- patch_shift(1L, shift = 1)
  expect_identical(get_patch_params(p)[[cols_param_name]], 1L)

  admit_columns(p, 2L, cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]], 1L)

  ## Shift patch on columns 3L & 5L.

  # Admitting a column at position 1L shifts the column indices in the result
  # up by one.
  p <- patch_shift(c(3L, 5L), shift = 1)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(3L, 5L))

  admit_columns(p, 1L, cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(4L, 6L))

  # Admitting a column at position 2L shifts the column indices in the result
  # up by one.
  p <- patch_shift(c(3L, 5L), shift = 1)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(3L, 5L))

  admit_columns(p, 2L, cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(4L, 6L))

  # Admitting a column at position 3L shifts the column indices in the result
  # up by one.
  p <- patch_shift(c(3L, 5L), shift = 1)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(3L, 5L))

  admit_columns(p, 3L, cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(4L, 6L))

  # Admitting a column at position 4L only affects the column indices greater
  # than 3L in the result.
  p <- patch_shift(c(3L, 5L), shift = 1)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(3L, 5L))

  admit_columns(p, 4L, cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(3L, 6L))

  # Admitting a column at position 5L only affects the column indices greater
  # than 4L in the result.
  p <- patch_shift(c(3L, 5L), shift = 1)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(3L, 5L))

  admit_columns(p, 5L, cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(3L, 6L))

  # Admitting a column at position 6L (or more) has no effect on the column
  # indices in the result (since the original column index was less than 6).
  p <- patch_shift(c(3L, 5L), shift = 1)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(3L, 5L))

  admit_columns(p, 6L, cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(3L, 5L))

  # Admitting a column at positions 1L & 3L shifts the column indices in the
  # result up by two.
  p <- patch_shift(c(3L, 5L), shift = 1)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(3L, 5L))

  admit_columns(p, c(1L, 3L), cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(5L, 7L))

  # Admitting a column at positions 1L & 4L shifts the column indices in the
  # result up by two.
  p <- patch_shift(c(3L, 5L), shift = 1)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(3L, 5L))

  admit_columns(p, c(1L, 4L), cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(5L, 7L))

  # Admitting a column at positions 1L & 5L shifts one column index in the
  # result up by one and the other up by two.
  p <- patch_shift(c(3L, 5L), shift = 1)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(3L, 5L))

  admit_columns(p, c(1L, 5L), cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(4L, 7L))

  # Admitting a column at positions 1L & 6L shifts one column index in the
  # result up by one and the other up by two.
  p <- patch_shift(c(3L, 5L), shift = 1)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(3L, 5L))

  admit_columns(p, c(1L, 6L), cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(4L, 7L))

  # Admitting a column at positions 1L & 7L shifts both column indices in the
  # result up by one.
  p <- patch_shift(c(3L, 5L), shift = 1)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(3L, 5L))

  admit_columns(p, c(1L, 7L), cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]], c(4L, 6L))

  ####
  #### Test with a permute patch.
  ####

  cols_param_name <- "perm"

  perm <- c(2L, 1L, 5L, 4L, 3L)

  # Admitting a column at position 1L shifts all column indices in the result
  # up by one.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, 1L, cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(1L, perm + 1L))

  # Admitting a column at position 2L.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, 2L, cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(3L, 2L, 1L, 6L, 5L, 4L))

  # Admitting a column at position 3L.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, 3L, cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(2L, 1L, 3L, 6L, 5L, 4L))

  # Admitting a column at position 4L.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, 4L, cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(2L, 1L, 6L, 4L, 5L, 3L))

  # Admitting a column at position 5L.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, 5L, cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(2L, 1L, 6L, 4L, 5L, 3L))

  # Admitting a column at position 6L.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, 6L, cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(2L, 1L, 5L, 4L, 3L, 6L))

  # Column position 7L (or more) is inadmissible.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  expect_error(admit_columns(p, 7L, cols_param_name = cols_param_name),
               regexp = "Inadmissible column")

  # Admitting columns at positions 1L & 2L.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, c(1L, 2L), cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(1L, 2L, 4L, 3L, 7L, 6L, 5L))

  # Admitting columns at positions 1L & 3L.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, c(1L, 3L), cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(1L, 4L, 3L, 2L, 7L, 6L, 5L))

  # Admitting columns at positions 1L & 4L.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, c(1L, 4L), cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(1L, 3L, 2L, 4L, 7L, 6L, 5L))

  # Admitting columns at positions 1L & 5L.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, c(1L, 5L), cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(1L, 3L, 2L, 7L, 5L, 6L, 4L))

  # Admitting columns at positions 1L & 6L.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, c(1L, 6L), cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(1L, 3L, 2L, 7L, 5L, 6L, 4L))

  # Admitting columns at positions 1L & 7L.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, c(1L, 7L), cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(1L, 3L, 2L, 6L, 5L, 4L, 7L))

  # Columns at position 1L & 8L (or more) are inadmissible.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  expect_error(admit_columns(p, c(1L, 8L), cols_param_name = cols_param_name),
               regexp = "Inadmissible column")

  # Admitting columns at positions 3L & 1L.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, c(3L, 1L), cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(1L, 4L, 3L, 2L, 7L, 6L, 5L))

  # Admitting columns at positions 2L & 3L.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, c(2L, 3L), cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(4L, 2L, 3L, 1L, 7L, 6L, 5L))

  # Admitting columns at positions 2L & 5L.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, c(2L, 5L), cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(3L, 2L, 1L, 7L, 5L, 6L, 4L))

  # Admitting columns at positions 1L, 3L & 5L.
  p <- patch_permute(perm)
  expect_identical(get_patch_params(p)[[cols_param_name]], expected = perm)
  admit_columns(p, c(1L, 3L, 5L), cols_param_name = cols_param_name)
  expect_identical(get_patch_params(p)[[cols_param_name]],
                   expected = c(1L, 4L, 3L, 2L, 5L, 8L, 7L, 6L))

  #!!!!!!!!!!!!!!!!
  ########## MOST IMP TODO. TEST admit_columns WITH THE INSERT PATCH TYPE.
  #!!!!!!!!!!!!!!!!

  ####
  #### Test with the identity patch.
  ####

  # Admitting any column position has no effect, but the cols argument must
  # have integer type.
  p <- patch_identity()

  expect_true(is_identity_patch(p))

  admit_columns(p, cols = 1L)
  expect_true(is_identity_patch(p))

  admit_columns(p, cols = 22L)
  expect_true(is_identity_patch(p))

  expect_error(admit_columns(p, cols = "x"))
})
