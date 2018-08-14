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

  patch_generators = list(gen_patch_affine, gen_patch_recode)

  penalty_scaling <- purrr::partial(ks_scaling, nx = nrow(df1), ny = nrow(df2))
  result <- columnwise_candidates(df1, df2 = df2,
                                  mismatch = diffness,
                                  patch_generators = patch_generators,
                                  patch_penalties = c(0.6, 0.6),
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
                                  patch_generators = patch_generators,
                                  patch_penalties = c(0.6, 0.6),
                                  break_penalty = 0.99,
                                  penalty_scaling = penalty_scaling)

  expect_true(is.list(result))
  expect_equal(length(unique(purrr::map_int(result, length))), expected = 1)
  expect_identical(length(result), ncol(df1[cols]))
  expect_identical(unique(purrr::map_int(result, length)), ncol(df2))

  result <- columnwise_candidates(df1, df2 = df2[cols],
                                  mismatch = diffness,
                                  patch_generators = patch_generators,
                                  patch_penalties = c(0.6, 0.6),
                                  break_penalty = 0.99,
                                  penalty_scaling = penalty_scaling)

  expect_true(is.list(result))
  expect_equal(length(unique(purrr::map_int(result, length))), expected = 1)
  expect_identical(length(result), ncol(df1))
  expect_identical(unique(purrr::map_int(result, length)), ncol(df2[cols]))

  # #### OPTIMISATION 05/12/2017:
  #
  # ####
  # #### Test with the UCI "adult" dataset.
  # ####
  # df <- read_data("adult", source = "uci")
  # dfs <- split_data(df, split = 0.5)
  # df1 <- dfs[[1]]
  # df2 <- dfs[[2]]
  #
  # patch_generators = list(gen_patch_affine, gen_patch_recode)
  #
  # penalty_scaling <- purrr::partial(ks_scaling, nx = nrow(df1), ny = nrow(df2))
  # result <- columnwise_candidates(df1, df2 = df2,
  #                                 mismatch = diffness,
  #                                 patch_generators = patch_generators,
  #                                 patch_penalties = c(0.6, 0.6),
  #                                 break_penalty = 0.99,
  #                                 penalty_scaling = penalty_scaling,
  #                                 verbose = TRUE)

  # Observation: a large proportion of execution time is spent on steps [i, 3],
  # for each i = 1,...,15.

  # The explanation is as follows:
  # Column 3 ("fnlwgt") in the UCI adult dataset contains integer data with
  # 21648 unique values (out of 32561 in total). Since integers are treated as
  # discrete data (at 05/12/2017), the gen_patch_recode function is called on
  # this column, which applies the Hungarian algorithm.

  # See issue #29.

})
