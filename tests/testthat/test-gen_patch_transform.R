require(testthat)
context("gen_patch_transform")

test_that("the gen_patch_transform function works", {

  generate_test_df <- function(n = 100) {
    v1 <- rnorm(n)
    v2 <- rnorm(n, sd = 3, mean = 4)
    v3 <- rexp(n)
    v4 <- sample.int(10, size = n, replace = TRUE)
    v5 <- sample(c("M", "F"), size = n, replace = TRUE, prob = c(1/4, 3/4))
    data.frame("v1" = v1, "v2" = v2, "v3" = v3, "v4" = v4, "v5" = v5,
               stringsAsFactors = FALSE)
  }

  set.seed(22)
  df1 <- generate_test_df()
  df2 <- generate_test_df()

  ## Test the case of continuous columns.
  result <- gen_patch_transform(df1, col1 = 1L, df2 = df2)
  expect_true(is_patch(result, allow_composed = TRUE))
  expect_identical(patch_type(result), c("scale", "shift"))
  expect_equal(get_patch_params(result)[[1]][["scale_factor"]],
               expected = 1, tolerance = 0.1)
  expect_equal(get_patch_params(result)[[2]][["shift"]],
               expected = 0, tolerance = 0.1)

  result <- gen_patch_transform(df1, col1 = 1L, df2 = df2, col2 = 2L)
  expect_true(is_patch(result, allow_composed = TRUE))
  expect_identical(patch_type(result), c("scale", "shift"))
  expect_equal(get_patch_params(result)[[1]][["scale_factor"]],
               expected = 3, tolerance = 0.2)
  expect_equal(get_patch_params(result)[[2]][["shift"]],
               expected = 4, tolerance = 0.1)

  ## Test the case of categorical columns.
  result <- gen_patch_transform(df1, col1 = 4L, df2 = df2, col2 = 4L)
  expect_true(is_patch(result, allow_composed = FALSE))
  expect_identical(patch_type(result), "recode")

  ## Test the case of incompatible columns.
  result <- gen_patch_transform(df1, col1 = 1L, df2 = df2, col2 = 4L)
  expect_null(result)

})
