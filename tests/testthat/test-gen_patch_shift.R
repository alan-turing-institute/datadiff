require(testthat)
context("gen_patch_shift")

test_that("the gen_patch_shift function works", {

  set.seed(2222)

  df1 <- data.frame(x = rnorm(500, mean = 2))
  df2 <- data.frame(x = rnorm(1000, mean = 10))

  result <- gen_patch_shift(df1, col1 = 1L, df2 = df2)

  expect_true(is_patch(result))
  expect_identical(patch_type(result), expected = "shift")
  expect_equal(get_patch_params(result)[["shift"]], expected = 8,
               tolerance = 0.1)

})
