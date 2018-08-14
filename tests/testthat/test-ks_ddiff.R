require(testthat)
context("ks_ddiff")

test_that("the ks_ddiff function works", {

  generate_normal_df <- function(n, mu_shift) {
    v1 <- rnorm(n, mean = mu_shift)
    v2 <- rnorm(n, mean = mu_shift, sd = 4)
    v3 <- rnorm(n, mean = 2 + mu_shift)
    v4 <- rnorm(n, sd = 2, mean = 4 + mu_shift)
    v5 <- rnorm(n, sd = 4, mean = 8 + mu_shift)
    data.frame("v1" = v1, "v2" = v2, "v3" = v3, "v4" = v4, "v5" = v5)
  }

  set.seed(14722)
  df1 <- generate_normal_df(500, mu_shift = 0)
  df2 <- generate_normal_df(501, mu_shift = 0.5)

  patch_generators = list(gen_patch_rescale, gen_patch_recode)

  # With a high confidence level (99%) the first-pass K-S test rejects the
  # null only on columns 1 and 3.
  alpha <- 0.01

  # NOTE: set verbose = TRUE for a confirmation message of K-S test result.
  result <- ks_ddiff(df1, df2 = df2, alpha = alpha,
                     patch_generators = patch_generators,
                     patch_penalties = c(0.6, 0.6), break_penalty = 0.99,
                     permute_penalty = 0.1)

  expect_true(is_patch(result))

  # With a lower confidence level (only 50%) the K-S test rejects the null on
  # all all columns. All columns are therefore included in the call to ddiff,
  # so the following call has a longer execution time, although the result is
  # in the following call is identical.
  alpha <- 0.5

  result <- ks_ddiff(df1, df2 = df2, alpha = alpha,
                     patch_generators = patch_generators,
                     patch_penalties = c(0.6, 0.6), break_penalty = 0.99,
                     permute_penalty = 0.1)

  expect_true(is_patch(result))
})
