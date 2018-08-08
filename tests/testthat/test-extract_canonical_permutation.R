require(testthat)
context("extract_canonical_permutation function")

test_that("the extract_canonical_permutation function works", {

  patch <- patch_permute(perm = 1:10)

  result <- extract_canonical_permutation(patch)
  expected <- patch
  expect_equal(result, expected = expected)
  expect_equal(get_patch_params(result)[["perm"]],
               expected = get_patch_params(expected)[["perm"]])


  patch <- patch_scale(cols = 2L, scale_factor = 22)
  expect_equal(extract_canonical_permutation(patch), expected = NA)


  patch <- compose_patch(patch_permute(perm = c(2:10, 1L)),
                          patch_scale(cols = 2L, scale_factor = 22))

  result <- extract_canonical_permutation(patch)
  expected <- decompose_patch(patch)[[2]]
  expect_equal(result, expected = expected)
  expect_equal(get_patch_params(result)[["perm"]],
               expected = get_patch_params(expected)[["perm"]])

  patch <- compose_patch(patch_permute(perm = 1:10),
                          patch_scale(cols = 2L, scale_factor = 22),
                          patch_insert(insertion_point = 0L, data = data.frame()),
                          patch_scale(cols = 3L, scale_factor = 33),
                          patch_shift(cols = 2L, shift = 22),
                          patch_insert(insertion_point = 4L, data = data.frame()))

  result <- extract_canonical_permutation(patch)
  expected <- decompose_patch(patch)[[6]]
  expect_equal(result, expected = expected)
  expect_equal(get_patch_params(result)[["perm"]],
               expected = get_patch_params(expected)[["perm"]])


  # This is the first non-trivial test (since the insert comes *after* the perm).
  # Here we permute and then insert at the first column position.
  patch <- compose_patch(patch_scale(cols = 2L, scale_factor = 22),
                         patch_insert(insertion_point = 0L, data = data.frame(1)),
                         patch_permute(perm = 1:10))
  expected <- patch_permute(perm = 1:11)
  result <- extract_canonical_permutation(patch)
  expect_equal(result, expected = expected)
  expect_equal(get_patch_params(result)[["perm"]],
               expected = get_patch_params(expected)[["perm"]])

  # Check that the definition of "canonical permutation" is satisfied.
  data <- 1:10
  names(data) <- paste0("v", 1:10)
  df <- data.frame(t(data))

  equivalent <- compose_patch(result,
                              patch_scale(cols = 2L, scale_factor = 22),
                              patch_insert(insertion_point = 0L, data = data.frame(1)))

  expect_identical(colnames(equivalent(df)), colnames(patch(df)))

  # Here we permute and then insert after column 4L.
  patch <- compose_patch(patch_scale(cols = 2L, scale_factor = 22),
                         patch_insert(insertion_point = 4L, data = data.frame(1)),
                         patch_permute(perm = 1:10))

  canonical_perm <- 1:11
  expected <- patch_permute(perm = canonical_perm)
  result <- extract_canonical_permutation(patch)
  expect_equal(result, expected = expected)
  expect_equal(get_patch_params(result)[["perm"]],
               expected = get_patch_params(expected)[["perm"]])

  # Check that the definition of "canonical permutation" is satisfied.
  data <- 1:10
  names(data) <- paste0("v", 1:10)
  df <- data.frame(t(data))

  equivalent <- compose_patch(expected,
                     patch_scale(cols = 2L, scale_factor = 22),
                     patch_insert(insertion_point = 4L, data = data.frame(1)))

  expect_identical(colnames(equivalent(df)), colnames(patch(df)))


  # Now test a non-trivial permutation. Here we permute and then insert.
  perm <- c(2:10, 1L)
  patch <- compose_patch(patch_scale(cols = 2L, scale_factor = 22),
                         patch_insert(insertion_point = 4L, data = data.frame(1)),
                         patch_permute(perm = perm))

  # The canonical permutation is the equivalent one if we were to permute
  # *after* insertion. Note that 6L here is the original 5th column, which is
  # permutated to position 4L (by patch) and stays there after the insertion.
  canonical_perm <- c(2L, 3L, 4L, 6L, 5L, 7L, 8L, 9L, 10L, 11L, 1L)
  expected <- patch_permute(perm = canonical_perm)
  result <- extract_canonical_permutation(patch)
  expect_equal(result, expected = expected)
  expect_equal(get_patch_params(result)[["perm"]],
               expected = get_patch_params(expected)[["perm"]])

  # Check that the definition of "canonical permutation" is satisfied.
  data <- 1:10
  names(data) <- paste0("v", 1:10)
  df <- data.frame(t(data))

  equivalent <- compose_patch(expected,
                              patch_scale(cols = 2L, scale_factor = 22),
                              patch_insert(insertion_point = 4L, data = data.frame(1)))

  expect_identical(colnames(equivalent(df)), colnames(patch(df)))


  # Now test with a delete component.
  # Now test a non-trivial permutation. Here we permute and then delete.
  perm <- c(2:10, 1L)
  patch <- compose_patch(patch_scale(cols = 2L, scale_factor = 22),
                         patch_delete(cols = 4L),
                         patch_permute(perm = perm))

  # The canonical permutation is the equivalent one if we were to permute
  # *after* deletion.
  canonical_perm <- c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L)
  expected <- patch_permute(perm = canonical_perm)
  result <- extract_canonical_permutation(patch)
  expect_equal(result, expected = expected)
  expect_equal(get_patch_params(result)[["perm"]],
               expected = get_patch_params(expected)[["perm"]])

  # Check that the definition of "canonical permutation" is satisfied.
  data <- 1:10
  names(data) <- paste0("v", 1:10)
  df <- data.frame(t(data))

  # Note that to construct the equivalent patch we must delete the column that's
  # in position 4L after the original permutation (with parameter 'perm'), namely
  # column v5.
  equivalent <- compose_patch(expected,
                              patch_scale(cols = 2L, scale_factor = 22),
                              patch_delete(cols = 5L))

  expect_identical(colnames(equivalent(df)), colnames(patch(df)))

  # TODO: test with a mixture of deletes, inserts and permutes.
})
