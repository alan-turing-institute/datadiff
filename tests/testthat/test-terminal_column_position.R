require(testthat)
context("terminal_column_position function")

test_that("the terminal_column_position function works", {

  ####
  #### Test with initial = TRUE
  ####
  patch <- patch_identity()
  column_index <- 2L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 2L)

  patch <- patch_shift(cols = 2L, shift = 22)
  column_index <- 2L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 2L)

  patch <- patch_shift(cols = 4L, shift = 22)
  column_index <- 2L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 2L)

  # Permute patch.

  patch <- patch_permute(as.integer(c(5, 3, 4, 2, 1)))

  column_index <- 1L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 5L)
  column_index <- 2L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 3L)
  column_index <- 3L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 4L)
  column_index <- 4L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 2L)
  column_index <- 5L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 1L)
  column_index <- 6L
  expect_error(terminal_column_position(column_index, patch = patch, initial = TRUE),
               regexp = "incompatible with permutation")

  # Insert patch.

  patch <- patch_insert(insertion_point = 2L, data = data.frame())
  column_index <- 1L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 1L)

  column_index <- 2L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 2L)

  # Test when the given column (after patch application) did not exist initially.
  column_index <- 3L
  expect_error(terminal_column_position(column_index, patch = patch, initial = TRUE),
               regexp = "corresponds to an inserted column")

  column_index <- 4L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 3L)

  # Delete patch.

  patch <- patch_delete(cols = 4L)
  column_index <- 3L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 3L)

  column_index <- 4L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 5L)

  # Composed patches.

  patch <- compose_patch(patch_insert(insertion_point = 3L, data = data.frame(1:10)),
                         patch_permute(as.integer(c(5, 3, 4, 2, 1))),
                         patch_delete(1L),
                         patch_scale(3:5, scale_factor = 10))

  column_index <- 1L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 6L)

  column_index <- 2L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 4L)

  column_index <- 3L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 5L)

  column_index <- 4L
  expect_error(terminal_column_position(column_index, patch = patch, initial = TRUE),
               regexp = "corresponds to an inserted column")

  column_index <- 5L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 3L)

  column_index <- 6L
  result <- terminal_column_position(column_index, patch = patch, initial = TRUE)
  expect_equal(result, expected = 2L)

  ####
  #### Test with initial = FALSE
  ####
  patch <- patch_identity()
  column_index <- 2L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 2L)

  patch <- patch_shift(cols = 2L, shift = 22)
  column_index <- 2L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 2L)

  patch <- patch_shift(cols = 4L, shift = 22)
  column_index <- 2L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 2L)

  # Test when the column_index is 0L (relevant when column index parameter is
  # the insertion_point in an insert patch).
  column_index <- 0L
  expect_error(terminal_column_position(column_index, patch = patch, initial = FALSE),
               regexp = "is not TRUE")

  # Permute patch.

  patch <- patch_permute(as.integer(c(5, 3, 4, 2, 1)))

  column_index <- 1L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 5L)
  column_index <- 2L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 4L)
  column_index <- 3L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 2L)
  column_index <- 4L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 3L)
  column_index <- 5L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 1L)
  column_index <- 6L
  expect_error(terminal_column_position(column_index, patch = patch, initial = FALSE),
               regexp = "incompatible with permutation")

  # Insert patch.

  patch <- patch_insert(insertion_point = 2L, data = data.frame())
  column_index <- 1L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 1L)

  column_index <- 2L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 2L)

  column_index <- 3L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 4L)

  # Delete patch.

  patch <- patch_delete(cols = 4L)
  column_index <- 3L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 3L)

  # Test when the given column (after patch application) no longer exists.
  column_index <- 4L
  expect_error(terminal_column_position(column_index, patch = patch, initial = FALSE),
               regexp = "corresponds to a deleted column")

  column_index <- 5L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 4L)

  # Composed patches.

  patch <- compose_patch(patch_insert(insertion_point = 3L, data = data.frame(1:10)),
                         patch_permute(as.integer(c(5, 3, 4, 2, 1))),
                         patch_delete(1L),
                         patch_scale(3:5, scale_factor = 10))

  column_index <- 1L
  expect_error(terminal_column_position(column_index, patch = patch, initial = FALSE),
               regexp = "corresponds to a deleted column")

  column_index <- 2L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 6L)

  column_index <- 3L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 5L)

  column_index <- 4L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 2L)

  column_index <- 5L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 3L)

  column_index <- 6L
  result <- terminal_column_position(column_index, patch = patch, initial = FALSE)
  expect_equal(result, expected = 1L)

  # Patch taken from batch experiment run on 22/01/2018.
  # wine-quality dataset, experiment 12, result 2.
  p1 = patch_rescale(cols = 5L, shift = -31.58284, scale_factor = 3876.67)
  p2 = patch_rescale(cols = 7L, shift = 0.01010382, scale_factor = 0.0004900724)

  patch <- compose_patch(p2, p1)

  result <- terminal_column_position(5L, patch, initial = TRUE)
  expect_equal(result, 5L)
  result <- terminal_column_position(7L, patch, initial = TRUE)
  expect_equal(result, 7L)

  result <- terminal_column_position(5L, patch, initial = FALSE)
  expect_equal(result, 5L)
  result <- terminal_column_position(7L, patch, initial = FALSE)
  expect_equal(result, 7L)

  # Now include a permutation patch which swaps columns 5 & 7.
  p3 = patch_permute(perm = as.integer(c(1, 2, 3, 4, 7, 6, 5, 8, 9, 10, 11, 12)))
  patch <- compose_patch(p3, p2, p1)

  result <- terminal_column_position(5L, patch, initial = TRUE)
  expect_equal(result, 7L)
  result <- terminal_column_position(7L, patch, initial = TRUE)
  expect_equal(result, 5L)

  result <- terminal_column_position(5L, patch, initial = FALSE)
  expect_equal(result, 7L)
  result <- terminal_column_position(7L, patch, initial = FALSE)
  expect_equal(result, 5L)

})
