require(testthat)
context("columnwise_ks")

test_that("the columnwise_ks function works", {

  generate_normal_df <- function(n) {
    v1 <- rnorm(n)
    v2 <- rnorm(n, sd = 4)
    v3 <- rnorm(n, mean = 2)
    v4 <- rnorm(n, sd = 2, mean = 4)
    v5 <- rnorm(n, sd = 4, mean = 8)
    data.frame("v1" = v1, "v2" = v2, "v3" = v3, "v4" = v4, "v5" = v5)
  }

  set.seed(14722)
  df1 <- generate_normal_df(100)
  df2 <- generate_normal_df(101)

  expect_error(columnwise_ks(df1, df2 = df1, alpha = 0))

  # Test when the K-S statistic is zero.
  result <- columnwise_ks(df1, df2 = df1, alpha = 0.1)
  expect_equal(result, expected = rep(FALSE, times = ncol(df1)))

  result <- columnwise_ks(df1, df2 = df1, alpha = 1e-10)
  expect_equal(result, expected = rep(FALSE, times = ncol(df1)))

  # Test when the K-S statistic is non-zero.
  # As alpha gets closer to zero, the K-S statistic must get larger for
  # rejection of the null hypothesis.

  # When the confidence level is _very_ strict (~100%), the null hypothesis is
  # _not_ rejected (they _might_ be sampled from the same distribution).
  result <- columnwise_ks(df1, df2 = df1, alpha = 1e-10)
  expect_equal(result, expected = rep(FALSE, times = ncol(df1)))

  # When the confidence level is _very_ lax (20%), the null hypothesis is
  # _always_ rejected, i.e. the columns are deemed, incorrectly, to be sampled
  # from different distributions.
  result <- columnwise_ks(df1, df2 = df2, alpha = 0.8)
  expect_equal(result, expected = rep(TRUE, times = ncol(df1)))

  # At the 90% confidence level the null is rejected only for column index 2.
  # This is a false positive.
  result <- columnwise_ks(df1, df2 = df2, alpha = 0.1)
  expect_equal(result, expected = c(F, T, F, F, F))

  # At the 99% confidence level the null is not rejected on any columns.
  # This makes sense, since the underlying distributions are indeed identical.
  result <- columnwise_ks(df1, df2 = df2, alpha = 0.01)
  expect_equal(result, expected = c(F, F, F, F, F))

  ####
  #### Now test with different underlying distributions
  ####

  generate_normal_df_ <- function(n, mu, sigma) {
    v1 <- rnorm(n, mean = mu, sd = sigma)
    v2 <- rnorm(n, sd = 4)
    v3 <- rnorm(n, mean = 2)
    v4 <- rnorm(n, sd = 2, mean = 4)
    v5 <- rnorm(n, sd = 4, mean = 8)
    data.frame("v1" = v1, "v2" = v2, "v3" = v3, "v4" = v4, "v5" = v5)
  }

  set.seed(14722)
  df1 <- generate_normal_df_(100, mu = 0, sigma = 1)
  df2 <- generate_normal_df_(101, mu = 1, sigma = 1)

  # At the 99.9% confidence level the null is rejected only for the column index
  # whose distibutions differ in d1 & df2.
  result <- columnwise_ks(df1, df2 = df2, alpha = 0.001)
  expect_equal(result, expected = c(T, F, F, F, F))

  set.seed(14722)
  df1 <- generate_normal_df_(100, mu = 0, sigma = 1)
  df2 <- generate_normal_df_(101, mu = 0.6, sigma = 1)

  # At the 99.9% confidence level the null is rejected only for the column index
  # whose distibutions differ in d1 & df2.
  result <- columnwise_ks(df1, df2 = df2, alpha = 0.001)
  expect_equal(result, expected = c(T, F, F, F, F))

  set.seed(14722)
  df1 <- generate_normal_df_(100, mu = 0, sigma = 1)
  df2 <- generate_normal_df_(101, mu = 0.5, sigma = 1)

  # Here the 99.9% confidence level the null is rejected on all columns, despite
  # the difference in the distributions.
  # NOTE: This case represents the risk of "accepting the null". Here we fail to
  # reject, yet the underlying distributions do in fact differ. As we see below,
  # this risk is mitigated as the sample size increases.
  result <- columnwise_ks(df1, df2 = df2, alpha = 0.001)
  expect_equal(result, expected = c(F, F, F, F, F))

  # NOTE: Increasing the confidence level only exaccerbates the risk described
  # above: "higher confidence" means more certainty against type 1 errors, i.e.
  # rejection of the null hypothesis is *less* likely.
  result <- columnwise_ks(df1, df2 = df2, alpha = 1e-10)
  expect_equal(result, expected = c(F, F, F, F, F))

  # IMP NOTE: Therefore, to be *more conservative* we must *reduce* the confidence
  # level (thereby making rejection of the null more likely, hence "accepting"
  # it less likely).
  # By reducing to 99% confidence we catch one of the columns that was previously
  # a K-S test false negative.
  result <- columnwise_ks(df1, df2 = df2, alpha = 0.01)
  expect_equal(result, expected = c(T, F, F, F, F))

  # Same thing at 95%
  result <- columnwise_ks(df1, df2 = df2, alpha = 0.05)
  expect_equal(result, expected = c(T, F, F, F, F))

  # And by 90% we have a false positive on column 2 (where the underlying
  # distributions are in fact identical).
  result <- columnwise_ks(df1, df2 = df2, alpha = 0.1)
  expect_equal(result, expected = c(T, T, F, F, F))

  # Now test with the same discrepancy in the mean parameter (mu), but with a
  # larger sample size.
  set.seed(14722)
  df1 <- generate_normal_df_(10000, mu = 0, sigma = 1)
  df2 <- generate_normal_df_(10001, mu = 0.5, sigma = 1)

  # Now at the 99.9% confidence level the null is rejected only for the column index
  # whose distibutions differ in d1 & df2.
  result <- columnwise_ks(df1, df2 = df2, alpha = 0.001)
  expect_equal(result, expected = c(T, F, F, F, F))

  # Same thing at 99%.
  result <- columnwise_ks(df1, df2 = df2, alpha = 0.01)
  expect_equal(result, expected = c(T, F, F, F, F))

  # This time, at 90%, the first and third columns are correctly rejected.
  result <- columnwise_ks(df1, df2 = df2, alpha = 0.1)
  expect_equal(result, expected = c(T, F, T, F, F))

  # However, the false negatives on columns 4 and 5 remain.

  # Now test with the same discrepancy in the mean parameter (mu), but with a
  # 'large-ish' sample size.
  set.seed(14722)
  df1 <- generate_normal_df_(1000, mu = 0, sigma = 1)
  df2 <- generate_normal_df_(1001, mu = 0.5, sigma = 1)

  # Now at the 99.9% confidence level the null is rejected only for the column index
  # whose distibutions differ in d1 & df2.
  result <- columnwise_ks(df1, df2 = df2, alpha = 0.001)
  expect_equal(result, expected = c(T, F, F, F, F))

  # With the large-ish sample size, the rejection of the null on that column persists
  # even at very high confidence levels.
  result <- columnwise_ks(df1, df2 = df2, alpha = 1e-10)
  expect_equal(result, expected = c(T, F, F, F, F))

  # Now test with the same discrepancy in the mean parameter (mu), but with a
  # 'small-ish' sample size.
  set.seed(14722)
  df1 <- generate_normal_df_(200, mu = 0, sigma = 1)
  df2 <- generate_normal_df_(201, mu = 0.5, sigma = 1)

  # Now at the 99.9% confidence level the null is rejected only for the column index
  # whose distibutions differ in d1 & df2.
  result <- columnwise_ks(df1, df2 = df2, alpha = 0.001)
  expect_equal(result, expected = c(T, F, F, F, F))

  # With the small-ish sample size, the rejection of the null on that column
  # fails at very high confidence levels.
  result <- columnwise_ks(df1, df2 = df2, alpha = 1e-10)
  expect_equal(result, expected = c(F, F, F, F, F))

})
