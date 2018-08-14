require(testthat)
context("metric_parameter_accuracy function")

test_that("the metric_parameter_accuracy function works", {

  datadiff <- purrr::partial(ddiff,
                             patch_generators = list(gen_patch_affine, gen_patch_recode),
                             patch_penalties = c(1, 1),
                             permute_penalty = 0.1)

  # Test with a dummy dataset.
  generate_df <- function(n) {
    v1 <- rnorm(n)
    v2 <- rnorm(n, sd = 5/4)
    v3 <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
    v4 <- rnorm(n, sd = 2, mean = 4)
    v5 <- rnorm(n, sd = 4, mean = 8)
    v6 <- sample(1:4, size = n, replace = TRUE)
    data.frame("v1" = v1, "v2" = v2, "v3" = v3, "v4" = v4, "v5" = v5, "v6" = v6)
  }

  set.seed(.Random.seed[1])
  data <- generate_df(500)

  seed <- 5432123
  N <- 2
  split <- 0.5
  corruption <- list(purrr::partial(sample_patch_permute, n = 2L))

  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  experiment <- execute_synthetic_experiment(config)

  result <- metric_parameter_accuracy(experiment)
  expect_equal(result, expected = NA)

  corruption <- list(purrr::partial(sample_patch_permute, n = 2L),
                     sample_patch_recode,
                     sample_patch_recode)

  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  experiment <- execute_synthetic_experiment(config)

  result <- metric_parameter_accuracy(experiment)

  # The result is a list:
  #   - list elements are named by the patch type and have an attribute
  #     containing the count of the number of terms in the sum of square errors.
  #   - list elements are numeric vectors
  #   - numeric vector elements are named by the real-valued parameter
  expect_true(is.list(result))
  expect_equal(names(result), expected = "recode")
  expect_equal(names(result[["recode"]]), expected = "encoding")

  # Four terms enter the computation of the mean parameter accuracy in this case
  # (since both results correctly identify both of the column recodings).
  expect_equal(attr(result[["recode"]], which = "count"), expected = 4L)

  # The recoding of the logical column (v3) is identified accurately in the first
  # result, but the recoding of column v6 in neither.
  expect_equal(result[["recode"]][["encoding"]], expected = (1 + 0 + 0 + 0)/4)

  ####
  #### Test with multiple patches of the same type applied to different columns
  #### and applied to the same column (fails with error "not columnwise unique"
  #### in the latter case).
  ####
  N <- 4
  corruption <- list(sample_patch_recode,
                     purrr::partial(sample_patch_permute, n = 2L),
                     sample_patch_recode)

  seed <- 703412341
  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)

  # This time, the corruption is column-wise unique (so we can compute the
  # column accuracy metric using our simple implementation).
  expect_true(is_columnwise_unique(config$get_corruption(), type = "recode"))
  experiment <- execute_synthetic_experiment(config)

  result <- metric_parameter_accuracy(experiment)

  expect_true(is.list(result))
  expect_equal(names(result), expected = "recode")
  expect_equal(names(result[["recode"]]), expected = "encoding")

  # Eight terms enter the computation of the mean parameter accuracy.
  expect_equal(attr(result[["recode"]], which = "count"), expected = 8L)
  # The Boolean recoding (v3) is accurately identified in three of the four results.
  # The other encoding (v6) is not accurately identified in any result.
  expected <- ((1 + 0 + 1 + 1) + (0 + 0 + 0 + 0))/8
  expect_equal(result[["recode"]][["encoding"]], expected = expected)

  ####
  #### Test with multiple patches of different types.
  ####
  N <- 4
  corruption <- list(purrr::partial(sample_patch_scale, mean = 100),
                     sample_patch_recode,
                     purrr::partial(sample_patch_permute, n = 2L),
                     sample_patch_recode)

  # Generate a larger dataset (to give the datadiff algorithm a better chance
  # of accurately identifying the recoding of column v6).
  set.seed(.Random.seed[1])
  data <- generate_df(2000)

  seed <- 123456789
  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)

  # The corruption is column-wise unique (so we can compute the
  # column accuracy metric using our simple implementation).
  expect_true(is_columnwise_unique(config$get_corruption(), type = "scale"))
  expect_true(is_columnwise_unique(config$get_corruption(), type = "recode"))
  experiment <- execute_synthetic_experiment(config)

  result <- metric_parameter_accuracy(experiment)

  expect_true(is.list(result))
  expect_equal(names(result), "recode")
  expect_equal(names(result[["recode"]]), expected = "encoding")

  # Eight terms enter the computation of average parameter accuracy.
  expect_equal(attr(result[["recode"]], which = "count"), expected = 8L)
  # The Boolean recoding (v3) is accurately identified in all of the four results.
  # The other encoding (v6) is accurately identified only in the first result.
  expected <- ((1 + 1 + 1 + 1) + (1 + 0 + 0 + 0))/8
  expect_equal(result[["recode"]][["encoding"]], expected = expected)

  ####
  #### Test when there are no patch types in the elementary decomposition
  #### of the corruption that contain matching parameters.
  ####
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

  result <- metric_parameter_accuracy(experiment)
  expect_equal(result, expected = NA)
})
