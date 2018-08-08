require(testthat)
context("false_positive_rate function")

test_that("the false_positive_rate function works", {

  # Test with a dummy dataset.
  generate_normal_df <- function(n) {
    v1 <- rnorm(n)
    v2 <- rnorm(n, sd = 5/4)
    v3 <- rnorm(n, mean = 1/50)
    v4 <- rnorm(n, sd = 2, mean = 4)
    v5 <- rnorm(n, sd = 4, mean = 8)
    data.frame("v1" = v1, "v2" = v2, "v3" = v3, "v4" = v4, "v5" = v5)
  }

  seed <- 147
  set.seed(seed)
  data <- generate_normal_df(500)
  datadiff <- purrr::partial(ddiff,
                             patch_generators = list(gen_patch_affine, gen_patch_recode))
  N <- 2
  split <- 0.5

  config <- configure_synthetic_experiment(data, datadiff = datadiff, N = N, seed = seed)
  experiment <- execute_synthetic_experiment(config)
  result <- false_positive_rate(experiment)

  expect_true(is.double(result))
  expect_equal(length(result), expected = 1)

  # Result is zero in this case.
  expect_equal(result, expected = 0)

  # Test with permute_penalty set to zero to get a non-zero false positive rate.
  seed <- 147
  set.seed(seed)
  data <- generate_normal_df(500)
  N <- 8

  datadiff <- purrr::partial(ddiff,
                             patch_generators = list(gen_patch_affine, gen_patch_recode),
                             permute_penalty = 0)
  config <- configure_synthetic_experiment(data, datadiff = datadiff,
                                           N = N, seed = seed)
  experiment <- execute_synthetic_experiment(config)
  result <- false_positive_rate(experiment)

  expect_true(is.double(result))
  expect_equal(length(result), expected = 1)
  expect_equal(result, 2/8)

  # Test with a non-trivial corruption. There is no false positive rate in this
  # case so the expected result is NA.
  corruption <- list(sample_patch_permute)
  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff,
                                           N = 2, seed = seed)
  experiment <- execute_synthetic_experiment(config)
  expect_equal(false_positive_rate(experiment), expected = NA)

  #### Test with the UCI abalone dataset with N = 10.
  data <- read_data("abalone", source = "uci")
  N <- 10
  seed <- .Machine$integer.max

  datadiff <- purrr::partial(ddiff,
                             patch_generators = list(gen_patch_affine, gen_patch_recode),
                             permute_penalty = 0, patch_penalties = c(0, 0))
  config <- configure_synthetic_experiment(data, datadiff = datadiff,
                                           N = N, seed = seed)
  experiment <- execute_synthetic_experiment(config)
  result <- false_positive_rate(experiment)

  # With the patch_penalties set to zero, the false positive rate is 1.
  expect_true(is.double(result))
  expect_equal(length(result), expected = 1)
  expect_equal(result, 1)

  # Now test the 'type' argument.
  has_permute <- purrr::map_lgl(experiment$get_results(),
                                .f = function(x) {
                                  "permute" %in% patch_type(x)
                                })
  expect_equal(false_positive_rate(experiment, type = "permute"),
               expected = c("permute" = sum(has_permute) / 10))

  has_shift <- purrr::map_lgl(experiment$get_results(),
                                .f = function(x) {
                                  "shift" %in% patch_type(x)
                                })
  expect_equal(false_positive_rate(experiment, type = "shift"),
               expected = c("shift" = sum(has_shift) / 10))

  has_scale <- purrr::map_lgl(experiment$get_results(),
                              .f = function(x) {
                                "scale" %in% patch_type(x)
                              })
  expect_equal(false_positive_rate(experiment, type = "scale"),
               expected = c("scale" = sum(has_scale) / 10))

  has_recode <- purrr::map_lgl(experiment$get_results(),
                              .f = function(x) {
                                "recode" %in% patch_type(x)
                              })
  expect_equal(false_positive_rate(experiment, type = "recode"),
               expected = c("recode" = sum(has_recode) / 10))

  # Test with a vector 'type' argument.
  expected <- c("shift" = sum(has_shift), "permute" = sum(has_permute), "recode" = sum(has_recode))/10
  expect_equal(false_positive_rate(experiment, type = c("shift", "permute", "recode")),
              expected = expected)

  # Test with an invalid 'type' argument.
  # NOTE: since we have no definitive list of valid patch types, and the
  # experiment results may or may not contain all relevant patch types (i.e.
  # a type for which the false positive rate is zero will not appear in the
  # results), any type which is not encountered will be deemed to have a false
  # positive rate of zero.
  #
  # To avoid errors due to misspelt type arguments, create a patch of the
  # relevant type and call patch_type on it.
  expect_equal(false_positive_rate(experiment, type = "xyz"),
               expected = c("xyz" = 0))
})
