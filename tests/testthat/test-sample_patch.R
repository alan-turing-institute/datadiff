require(testthat)
context("sample_patch function")

test_that("the sample_patch function works", {

  result <- sample_patch(mtcars, sample_patch_identity)
  expect_true(is_identity_patch(result))

  result <- sample_patch(mtcars, sample_patch_identity, sample_patch_identity)
  expect_true(is_identity_patch(result))

  result <- sample_patch(mtcars, sample_patch_permute)
  expect_true(is_patch(result))
  expect_equal(patch_type(result), expected = "permute")

  result <- sample_patch(mtcars, sample_patch_delete, sample_patch_permute)
  expect_true(is_patch(result))
  expect_equal(patch_type(result), expected = c("delete", "permute"))

  # Use do.call if the sampler functions are in a list.
  sampler_list <- list(sample_patch_delete, sample_patch_permute, sample_patch_rescale)
  result <- do.call(sample_patch, args = c(list(df = mtcars), sampler_list))

  expect_true(is_patch(result))
  expect_equal(patch_type(result), expected = c("delete", "permute", "rescale"))

  ####
  #### Test the columnwise-uniqueness of the sampled patch.
  ####

  generate_mixed_df <- function(n = 100) {
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

  sampler_list <- list(sample_patch_recode, sample_patch_recode)
  result <- do.call(sample_patch, args = c(list(df = df), sampler_list))

  expect_true(is_patch(result))
  expect_equal(patch_type(result), expected = c("recode", "recode"))

  # Attempting to sample a patch with three recode components results in an
  # error since there are only two columns of categorical data, so the maximum
  # possible number of recode components, under the constraint that the result
  # must be column-wise unique, is two.
  sampler_list <- list(sample_patch_recode, sample_patch_recode, sample_patch_recode)
  expect_error(do.call(sample_patch, args = c(list(df = df), sampler_list)),
               regexp = "All candidate columns are excluded")


  ####
  #### Bugfix 10/01/2018. Observed with the UCI heart-diseaset dataset.
  ####
  set.seed <- 201801103

  df <-heartdisease

  # The use of purrr::partial to partially-fill arguments in the patch sampler
  # function caused a bug since it obfuscates the set of formal arguments to
  # the sampler function.
  rdist_scale <- purrr::partial(stats::rnorm, mean = 1.1, sd = 0.1)
  sample_patch_rescale1.1 <- purrr::partial(sample_patch_rescale, relative_shift = 0.1,
                                            rdist_scale = rdist_scale)

  sampler_list <- list(sample_patch_rescale1.1, sample_patch_rescale1.1)
  expect_error(do.call(sample_patch, args = c(list(df = df), sampler_list)),
               regexp = "All candidate columns are excluded")

})
