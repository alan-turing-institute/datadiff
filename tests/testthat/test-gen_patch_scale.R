require(testthat)
context("gen_patch_scale")

test_that("the gen_patch_scale function works", {

  set.seed(2222)

  df <- data.frame(x = rnorm(500, sd = 2))
  y <- rnorm(1000, sd = 10)

  result <- gen_patch_scale(df, cols = 1L, y = y)

  expect_true(is_patch(result))
  expect_identical(patch_type(result), expected = "scale")
  expect_equal(get_patch_params(result)[["scale_factor"]], expected = 5,
               tolerance = 0.2)

})
