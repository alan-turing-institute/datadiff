require(testthat)
context("gen_patch_perm")

test_that("the gen_patch_perm function works", {

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

  result <- gen_patch_perm(df1, df2)
  expect_true(is(result, "patch_identity"))

  perm <- c(3, 1, 2, 4, 5)
  result <- gen_patch_perm(df1[, perm], df2)
  expect_equal(perm, expected = get_patch_params(result)[["perm"]])

  perm <- c(4, 1, 5, 2, 3)
  result <- gen_patch_perm(df1[, perm], df2)
  expect_equal(perm, expected = get_patch_params(result)[["perm"]])

})
