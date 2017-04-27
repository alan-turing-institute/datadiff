require(testthat)
context("ddiff")

test_that("the ddiff function works", {

  generate_test_df <- function(n = 50) {
    v1 <- rnorm(n)
    v2 <- rnorm(n, sd = 4, mean = 1)
    v3 <- rexp(n)
    v4 <- sample.int(10, size = n, replace = TRUE)
    v5 <- sample(c("M", "F"), size = n, replace = TRUE, prob = c(1/4, 3/4))
    data.frame("v1" = v1, "v2" = v2, "v3" = v3, "v4" = v4, "v5" = v5,
               stringsAsFactors = FALSE)
  }

  set.seed(22)
  df1 <- generate_test_df(50)
  df2 <- generate_test_df(50)

  cost_transform <- 0.1
  cost_permute <- 0.1

  result <- ddiff(df1, df2 = df2, cost_permute = cost_permute,
                  cost_transform = cost_transform)

  expect_true(is_patch(result, allow_composed = TRUE))

  # TODO: unfinished - include some tests!

  # Test with the numeric columns permuted in df1 and a shift.
  df1[[1]] <- 4 + df1[[1]]
  df1 <- df1[c(2, 3, 1, 4, 5)]

  result <- ddiff(df1, df2 = df2, cost_permute = cost_permute,
                  cost_transform = cost_transform)

  # Test for the expected result.

  # Now test with a lower cost for a tranformation patch.
  cost_transform <- 0.01
  result <- ddiff(df1, df2 = df2, cost_permute = cost_permute,
                  cost_transform = cost_transform)

  # Observe that with this reduced cost, a recoding of column 4 is suggested.

  # TODO: test with data frames containing both factors and integer data (by
  # setting stringsAsFactors = TRUE above).


  ## TODO: test the composed argument.
})
