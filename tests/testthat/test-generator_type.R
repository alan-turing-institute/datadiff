require(testthat)
context("generator_type")

test_that("the generator_type function works", {

  ####
  #### Test with a dummy dataset.
  ####
  generate_normal_df <- function(n) {
    v1 <- rnorm(n)
    v2 <- rnorm(n, sd = 5/4)
    v3 <- rnorm(n, mean = 1/50)
    v4 <- rnorm(n, sd = 2, mean = 4)
    v5 <- rnorm(n, sd = 4, mean = 8)
    data.frame("v1" = v1, "v2" = v2, "v3" = v3, "v4" = v4, "v5" = v5)
  }

  set.seed(147)
  df <- generate_normal_df(100)

  set.seed(.Machine$integer.max)
  patch_generator <- gen_patch_rescale
  result <- generator_type(df, patch_generator = patch_generator)
  expect_equal(result, expected = "rescale")

  result <- generator_type(df, patch_generator = patch_generator, short = FALSE)
  expect_equal(result, expected = "patch_rescale")

  patch_generator <- list(gen_patch_scale, gen_patch_shift)
  result <- generator_type(df, patch_generator = patch_generator)
  expect_equal(result, expected = c("scale", "shift"))

  patch_generator <- list(gen_patch_shift, gen_patch_scale)
  result <- generator_type(df, patch_generator = patch_generator)
  expect_equal(result, expected = c("shift" ,"scale"))

  # Test with an incompatible patch type.
  patch_generator <- list(gen_patch_rescale, gen_patch_recode)
  result <- generator_type(df, patch_generator = patch_generator)
  expect_equal(result, expected = c("rescale", NA))

  ####
  #### Test with mixed data.
  ####
  generate_mixed_df <- function(n) {
    v1 <- rnorm(n)
    v2 <- rnorm(n, sd = 4, mean = 1)
    v3 <- rexp(n)
    v4 <- sample.int(10, size = n, replace = TRUE)
    v5 <- sample(c("M", "F"), size = n, replace = TRUE, prob = c(1/4, 3/4))
    data.frame("v1" = v1, "v2" = v2, "v3" = v3, "v4" = v4, "v5" = v5,
               stringsAsFactors = FALSE)
  }

  set.seed(22)
  df <- generate_mixed_df(100)

  set.seed(.Machine$integer.max)
  patch_generator <- gen_patch_rescale
  result <- generator_type(df, patch_generator = patch_generator)
  expect_equal(result, expected = "rescale")

  result <- generator_type(df, patch_generator = patch_generator, short = FALSE)
  expect_equal(result, expected = "patch_rescale")

  patch_generator <- list(gen_patch_scale, gen_patch_shift)
  result <- generator_type(df, patch_generator = patch_generator)
  expect_equal(result, expected = c("scale", "shift"))

  patch_generator <- list(gen_patch_shift, gen_patch_scale)
  result <- generator_type(df, patch_generator = patch_generator)
  expect_equal(result, expected = c("shift" ,"scale"))

  # The recode patch type is no longer incompatible.
  patch_generator <- list(gen_patch_rescale, gen_patch_recode)
  result <- generator_type(df, patch_generator = patch_generator)
  expect_equal(result, expected = c("rescale", "recode"))

  ##
  ## Test with generator types which are not "columnwise".
  ##
  patch_generator <- list(gen_patch_permute)
  expect_error(generator_type(df, patch_generator = patch_generator),
               regexp = "Only columnwise types are supported")


})
