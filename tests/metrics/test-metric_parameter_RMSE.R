require(testthat)
context("metric_parameter_RMSE function")

test_that("the metric_parameter_RMSE function works", {

  datadiff <- purrr::partial(ddiff,
                             patch_generators = list(gen_patch_affine, gen_patch_recode),
                             patch_penalties = c(1, 1),
                             permute_penalty = 0.1)

  # Test with a dummy dataset.
  generate_normal_df <- function(n) {
    v1 <- rnorm(n)
    v2 <- rnorm(n, sd = 5/4)
    v3 <- rnorm(n, mean = 1/50)
    v4 <- rnorm(n, sd = 2, mean = 4)
    v5 <- rnorm(n, sd = 4, mean = 8)
    data.frame("v1" = v1, "v2" = v2, "v3" = v3, "v4" = v4, "v5" = v5)
  }

  set.seed(.Random.seed[1])
  data <- generate_normal_df(500)
  seed <- 22
  N <- 2
  split <- 0.5
  corruption <- list(purrr::partial(sample_patch_permute, n = 2L),
                     purrr::partial(sample_patch_scale, mean = 10))

  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  experiment <- execute_synthetic_experiment(config)

  # The corruption is composed of a permutation and a scale patch.
  expect_equal(patch_type(experiment$get_corruption()),
               expected = c("permute", "scale"))

  # The ddiff algorithm proposes an affine (shift + scale) and a permutation.
  expect_true(all(purrr::map_lgl(experiment$results, .f = function(x) {
    identical(patch_type(x), c("scale", "shift", "permute"))
  })))

  result <- metric_parameter_RMSE(experiment)

  # The result is a list:
  #   - list elements are named by the patch type and have an attribute
  #     containing the count of the number of terms in the sum of square errors.
  #   - list elements are numeric vectors
  #   - numeric vector elements are named by the real-valued parameter
  expect_true(is.list(result))
  expect_equal(names(result), expected = "scale")
  expect_equal(names(result[["scale"]]), expected = "scale_factor")

  # Only one term enters the sum of square errors in this case (since the
  # scale applied to column v2 is correctly identified only in the first result).
  expect_equal(attr(result[["scale"]], which = "count"), expected = 1L)
  expect_equal(result[["scale"]][["scale_factor"]],
               expected = sqrt((10.593 - 9.497)^2), tolerance = 0.001)

  ####
  #### Test with multiple patches of the same type applied to different columns
  #### and applied to the same column (fails with error "not columnwise unique"
  #### in the latter case).
  ####
  N <- 4
  corruption <- list(purrr::partial(sample_patch_scale, mean = 100),
                     purrr::partial(sample_patch_permute, n = 2L),
                     purrr::partial(sample_patch_scale, mean = 10))

  seed <- 703412341
  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)

  expect_true(is_columnwise_unique(config$get_corruption(), type = "scale"))
  experiment <- execute_synthetic_experiment(config)

  result <- metric_parameter_RMSE(experiment)

  expect_true(is.list(result))
  expect_equal(names(result), expected = "scale")
  expect_equal(names(result[["scale"]]), expected = "scale_factor")

  # Three terms enter the sum of square errors, corresponding to the scaling
  # applied to column v4 (since this scaling is correctly identified
  # in only three of the four results).
  expect_equal(attr(result[["scale"]], which = "count"), expected = 3L)
  expected <- sqrt(((100.513 - 96.475)^2 +
                      (100.513 - 107.993)^2 + (100.513 - 114.145)^2)/3)
  expect_equal(result[["scale"]][["scale_factor"]],
               expected = expected, tolerance = 0.01)

  ####
  #### Test with multiple patches of different types.
  ####
  N <- 4
  corruption <- list(purrr::partial(sample_patch_scale, mean = 100),
                     purrr::partial(sample_patch_shift, mean = -40),
                     purrr::partial(sample_patch_permute, n = 2L),
                     purrr::partial(sample_patch_shift, mean = 10))

  seed <- 121212121
  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)

  # The corruption is column-wise unique (so we can compute the
  # column accuracy metric using our simple implementation).
  expect_true(is_columnwise_unique(config$get_corruption(), type = "scale"))
  expect_true(is_columnwise_unique(config$get_corruption(), type = "shift"))
  experiment <- execute_synthetic_experiment(config)

  result <- metric_parameter_RMSE(experiment)

  expect_true(is.list(result))
  expect_true(setequal(names(result), c("scale", "shift")))
  expect_equal(names(result[["scale"]]), expected = "scale_factor")
  expect_equal(names(result[["shift"]]), expected = "shift")

  # One term enters the sum of square errors for the scale corruption, since
  # the scale corruption on column 2 is only identified in one result.
  expect_equal(attr(result[["scale"]], which = "count"), expected = 1L)
  expected <- sqrt((99.542 - 102.325)^2)
  expect_equal(result[["scale"]][["scale_factor"]],
               expected = expected, tolerance = 0.001)

  # Eight terms enter the sum of square errors for the shift corruption.
  expect_equal(attr(result[["shift"]], which = "count"), expected = 7L)
  expected <- sqrt(((10.804 - 10.758)^2 +
                      (10.804 - 10.672)^2 + (10.804 - 10.8)^2 +
                      (-41.388 - (-41))^2 + (-41.388 - (-41.296))^2 +
                      (-41.388 - (-41.93))^2 + (-41.388 - -(41.476))^2)/7)
  expect_equal(result[["shift"]][["shift"]],
               expected = expected, tolerance = 0.01)


  ####
  #### Test when there are no patch types in the elementary decomposition
  #### of the corruption that contain real-valued parameters.
  ####
  generate_df <- function(n) {
    v1 <- rnorm(n)
    v2 <- rnorm(n, sd = 5/4)
    v3 <- rnorm(n, mean = 1/50)
    v4 <- rnorm(n, sd = 2, mean = 4)
    v5 <- rnorm(n, sd = 4, mean = 8)
    v6 <- sample(1:10, size = n, replace = TRUE)
    data.frame("v1" = v1, "v2" = v2, "v3" = v3, "v4" = v4, "v5" = v5, "v6" = v6)
  }

  set.seed(.Random.seed[1])
  data <- generate_df(500)
  seed <- 22
  N <- 2
  split <- 0.5
  corruption <- list(purrr::partial(sample_patch_permute, n = 2L))

  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  experiment <- execute_synthetic_experiment(config)

  result <- metric_parameter_RMSE(experiment)
  expect_equal(result, expected = NA)

  corruption <- list(purrr::partial(sample_patch_permute, n = 2L),
                     sample_patch_recode)

  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  experiment <- execute_synthetic_experiment(config)

  result <- metric_parameter_RMSE(experiment)
  expect_equal(result, expected = NA)
})
