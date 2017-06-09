require(testthat)
context("columnwise_candidates")

test_that("the columnwise_candidates function works", {

  generate_normal_df <- function(n) {
    v1 <- rnorm(n)
    v2 <- rnorm(n, sd = 4)
    v3 <- rnorm(n, mean = 2)
    v4 <- rnorm(n, sd = 2, mean = 4)
    v5 <- rnorm(n, sd = 4, mean = 8)
    data.frame("v1" = v1, "v2" = v2, "v3" = v3, "v4" = v4, "v5" = v5)
  }

  set.seed(22)
  df1 <- generate_normal_df(100)
  df2 <- generate_normal_df(101)

  penalty_scaling <- purrr::partial(ks_scaling, nx = nrow(df1), ny = nrow(df2))
  result <- columnwise_candidates(df1, df2 = df2,
                                  mismatch = diffness,
                                  patch_generators = list(gen_patch_transform),
                                  patch_penalties = 0.6,
                                  break_penalty = 0.99,
                                  penalty_scaling = penalty_scaling)

  expect_true(is.list(result))
  expect_equal(length(unique(purrr::map_int(result, length))), expected = 1)
  expect_identical(length(result), ncol(df1))
  expect_identical(unique(purrr::map_int(result, length)), ncol(df2))

  ## Test with differing numbers of columns.
  cols <- c(1, 2, 4)
  result <- columnwise_candidates(df1[cols], df2 = df2,
                                  mismatch = diffness,
                                  patch_generators = list(gen_patch_transform),
                                  patch_penalties = 0.6,
                                  break_penalty = 0.99,
                                  penalty_scaling = penalty_scaling)

  expect_true(is.list(result))
  expect_equal(length(unique(purrr::map_int(result, length))), expected = 1)
  expect_identical(length(result), ncol(df1[cols]))
  expect_identical(unique(purrr::map_int(result, length)), ncol(df2))

  result <- columnwise_candidates(df1, df2 = df2[cols],
                                  mismatch = diffness,
                                  patch_generators = list(gen_patch_transform),
                                  patch_penalties = 0.6,
                                  break_penalty = 0.99,
                                  penalty_scaling = penalty_scaling)

  expect_true(is.list(result))
  expect_equal(length(unique(purrr::map_int(result, length))), expected = 1)
  expect_identical(length(result), ncol(df1))
  expect_identical(unique(purrr::map_int(result, length)), ncol(df2[cols]))
})
