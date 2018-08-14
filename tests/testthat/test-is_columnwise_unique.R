require(testthat)
context("is_columnwise_unique function")

test_that("the is_columnwise_unique function works", {

  patch <- patch_identity()
  expect_error(is_columnwise_unique(patch, "identity"),
              regexp = "Column index parameter not found")

  patch <- patch_identity()
  expect_error(is_columnwise_unique(patch, "shift"),
               regexp = "does not contain any components of type")

  patch <- patch_shift(2L, shift = 10)
  expect_true(is_columnwise_unique(patch, type = "shift"))

  patch <- patch_scale(5L, scale_factor = 2)
  expect_true(is_columnwise_unique(patch, type = "scale"))

  patch <- compose_patch(patch_scale(5L, scale_factor = 2),
                          patch_shift(2L, shift = 10))
  expect_true(is_columnwise_unique(patch, type = "scale"))

  patch <- compose_patch(patch_scale(5L, scale_factor = 2),
                          patch_shift(5L, shift = 10))
  expect_true(is_columnwise_unique(patch, type = "shift"))

  patch <- compose_patch(patch_scale(5L, scale_factor = 2),
                          patch_shift(5L, shift = 10))
  expect_true(is_columnwise_unique(patch, type = "scale"))

  patch <- compose_patch(patch_scale(5L, scale_factor = 2),
                          patch_scale(5L, scale_factor = 10))
  expect_false(is_columnwise_unique(patch, type = "scale"))
  expect_error(is_columnwise_unique(patch, type = "shift"),
               regexp = "does not contain any components of type")

  # Test the 'short' argument.
  patch <- compose_patch(patch_scale(5L, scale_factor = 2),
                         patch_scale(5L, scale_factor = 10))
  expect_false(is_columnwise_unique(patch, type = "patch_scale", short = FALSE))

  #### Test the case where the column indices differ in the patch objects but
  #### they actually refer to the same column.

  patch <- compose_patch(patch_scale(5L, scale_factor = 2),
                          patch_permute(as.integer(c(1, 5, 3, 4, 2))),
                          patch_scale(2L, scale_factor = 10))
  expect_false(is_columnwise_unique(patch, type = "scale"))

  patch <- compose_patch(patch_scale(4L, scale_factor = 2),
                          patch_permute(as.integer(c(1, 5, 3, 4, 2))),
                          patch_scale(2L, scale_factor = 10))
  expect_true(is_columnwise_unique(patch, type = "scale"))

  # Test with a vector cols argument to the patch constructor.
  patch <- compose_patch(patch_scale(c(4L, 5L), scale_factor = 2),
                          patch_permute(as.integer(c(1, 5, 3, 4, 2))),
                          patch_scale(2L, scale_factor = 10))
  expect_false(is_columnwise_unique(patch, type = "scale"))

  # Test with an insert patch (which are always columnwise unique).
  patch <- patch_insert(insertion_point = 0L, data = data.frame(NA))
  expect_true(is_columnwise_unique(patch, type = "insert"))

  # Test with insert & delete patches.
  patch <- compose_patch(patch_scale(5L, scale_factor = 2),
                          patch_insert(insertion_point = 3L, data = data.frame()),
                          patch_scale(5L, scale_factor = 10))
  expect_true(is_columnwise_unique(patch, type = "scale"))

  patch <- compose_patch(patch_scale(6L, scale_factor = 2),
                          patch_insert(insertion_point = 3L, data = data.frame()),
                          patch_scale(5L, scale_factor = 10))
  expect_false(is_columnwise_unique(patch, type = "scale"))

  patch <- compose_patch(patch_scale(5L, scale_factor = 2),
                          patch_delete(cols = 3L),
                          patch_scale(5L, scale_factor = 10))
  expect_true(is_columnwise_unique(patch, type = "scale"))

  patch <- compose_patch(patch_scale(4L, scale_factor = 2),
                          patch_delete(cols = 4L),
                          patch_scale(4L, scale_factor = 10))
  expect_true(is_columnwise_unique(patch, type = "scale"))

  patch <- compose_patch(patch_scale(4L, scale_factor = 2),
                          patch_delete(cols = 4L),
                          patch_scale(5L, scale_factor = 10))
  expect_false(is_columnwise_unique(patch, type = "scale"))

  # Test with a combination of permutations and insert & delete patches.
  patch <- compose_patch(patch_scale(5L, scale_factor = 2),
                          patch_permute(as.integer(c(1, 5, 3, 4, 2))),
                          patch_delete(1L),
                          patch_scale(2L, scale_factor = 10))
  expect_true(is_columnwise_unique(patch, type = "scale"))

  patch <- compose_patch(patch_scale(1L, scale_factor = 2),
                          patch_permute(as.integer(c(1, 5, 3, 4, 2))),
                          patch_delete(1L),
                          patch_scale(2L, scale_factor = 10))
  expect_false(is_columnwise_unique(patch, type = "scale"))

  patch <- compose_patch(patch_scale(5L, scale_factor = 2),
                          patch_permute(as.integer(c(5, 3, 4, 2, 1))),
                          patch_delete(1L),
                          patch_scale(2L, scale_factor = 10))
  expect_false(is_columnwise_unique(patch, type = "scale"))

  # Insert & delete:
  patch <- compose_patch(patch_scale(6L, scale_factor = 2),
                          patch_insert(insertion_point = 3L, data = data.frame()),
                          patch_permute(as.integer(c(5, 3, 4, 2, 1))),
                          patch_delete(1L),
                          patch_scale(1L, scale_factor = 10))
  expect_true(is_columnwise_unique(patch, type = "scale"))

  patch <- compose_patch(patch_scale(6L, scale_factor = 2),
                          patch_insert(insertion_point = 3L, data = data.frame()),
                          patch_permute(as.integer(c(5, 3, 4, 2, 1))),
                          patch_delete(1L),
                          patch_scale(3L, scale_factor = 10))
  expect_true(is_columnwise_unique(patch, type = "scale"))

  patch <- compose_patch(patch_scale(6L, scale_factor = 2),
                          patch_insert(insertion_point = 3L, data = data.frame()),
                          patch_permute(as.integer(c(5, 3, 4, 2, 1))),
                          patch_delete(1L),
                          patch_scale(2L, scale_factor = 10))
  expect_false(is_columnwise_unique(patch, type = "scale"))

  # Test with patches having vector column index parameters.
  patch <- compose_patch(patch_scale(6L, scale_factor = 2),
                          patch_insert(insertion_point = 3L, data = data.frame()),
                          patch_permute(as.integer(c(5, 3, 4, 2, 1))),
                          patch_delete(1L),
                          patch_scale(2:3, scale_factor = 10))
  expect_false(is_columnwise_unique(patch, type = "scale"))

  patch <- compose_patch(patch_scale(6L, scale_factor = 2),
                          patch_insert(insertion_point = 3L, data = data.frame()),
                          patch_permute(as.integer(c(5, 3, 4, 2, 1))),
                          patch_delete(1L),
                          patch_scale(3:5, scale_factor = 10))
  expect_true(is_columnwise_unique(patch, type = "scale"))

  patch <- compose_patch(patch_scale(c(2L, 6L), scale_factor = 2),
                          patch_insert(insertion_point = 3L, data = data.frame()),
                          patch_permute(as.integer(c(5, 3, 4, 2, 1))),
                          patch_delete(1L),
                          patch_scale(3:5, scale_factor = 10))
  expect_false(is_columnwise_unique(patch, type = "scale"))

  ###
  ### Test with type = permute
  ###
  patch <- compose_patch(patch_scale(5L, scale_factor = 2),
                         patch_permute(as.integer(c(1, 5, 3, 4, 2))),
                         patch_scale(2L, scale_factor = 10))
  expect_error(is_columnwise_unique(patch, type = "permute"),
               regexp = "Column index parameter not found")

  ###
  ### Test with vector 'type' arguments
  ###
  patch <- compose_patch(patch_scale(6L, scale_factor = 2),
                         patch_insert(insertion_point = 3L, data = data.frame()),
                         patch_shift(4L, shift = 10),
                         patch_delete(1L),
                         patch_scale(1L, scale_factor = 10))
  expect_true(is_columnwise_unique(patch, type = "scale"))
  expect_true(is_columnwise_unique(patch, type = "shift"))

  expect_equal(length(is_columnwise_unique(patch, type = c("shift", "scale"))),
               expected = 2)
  expect_true(all(is_columnwise_unique(patch, type = c("shift", "scale"))))

  patch <- compose_patch(patch_scale(6L, scale_factor = 2),
                         patch_insert(insertion_point = 3L, data = data.frame()),
                         patch_shift(4L, shift = 10),
                         patch_delete(1L),
                         patch_scale(6L, scale_factor = 10))
  expect_false(is_columnwise_unique(patch, type = "scale"))
  expect_true(is_columnwise_unique(patch, type = "shift"))

  expect_equal(length(is_columnwise_unique(patch, type = c("shift", "scale"))),
               expected = 2)
  expect_false(all(is_columnwise_unique(patch, type = c("shift", "scale"))))
  expect_identical(is_columnwise_unique(patch, type = c("shift", "scale")),
                   expected = c("shift" = TRUE, "scale" = FALSE))

})
