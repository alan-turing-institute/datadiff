require(testthat)
context("matching_types function")

test_that("the matching_types function works", {

  patch <- patch_recode(cols=2L, encoding = c("a" = 1))

  expect_equal(matching_types(patch, param_name = "cols"), expected = "recode")
  expect_equal(matching_types(patch, param_name = "encoding"), expected = "recode")
  expect_equal(matching_types(patch, param_name = "shift"), expected = character(0))

  patch <- compose_patch(patch_permute(c(1L, 4L, 5L, 3L, 2L)),
                         patch_shift(cols=2L, shift = 3),
                         patch_insert(insertion_point = 0L, data = data.frame()),
                         patch_scale(cols=3L, scale_factor = 100),
                         patch_shift(cols=4L, shift = 1))

  expect_equal(matching_types(patch, param_name = "cols"),
               expected = c("shift", "scale"))
  expect_equal(matching_types(patch, param_name = "shift"),
               expected = "shift")
  expect_equal(matching_types(patch, param_name = "scale_factor"),
               expected = "scale")
  expect_equal(matching_types(patch, param_name = "encoding"),
               expected = character(0))

})
