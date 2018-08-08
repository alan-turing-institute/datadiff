require(testthat)
context("metric_type_precision function")

test_that("the metric_type_precision function works", {

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
  datadiff <- ddiff

  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  experiment <- execute_synthetic_experiment(config)

  count_attr <- "count"
  result <- metric_type_precision(experiment, short = TRUE, count_attr = count_attr)

  # The corruption is composed of a permutation and a scale patch.
  expect_equal(patch_type(experiment$get_corruption()),
               expected = c("permute", "scale"))

  # The ddiff algorithm proposes an affine (shift + scale) and a permutation.
  expect_true(all(purrr::map_lgl(experiment$results, .f = function(x) {
    identical(patch_type(x), c("scale", "shift", "permute"))
  })))

  # Two of the results contain a scale patch. So does the corruption.
  scale_result <- 1
  attr(scale_result, which = count_attr) <- 2

  # Two of the results contain a shift patch. The corruption does not.
  shift_result <- 0
  attr(shift_result, which = count_attr) <- 2

  # Two of the results contain a permutation patch. So does the corruption.
  perm_result <- 1
  attr(perm_result, which = count_attr) <- 2

  expected <- list(
    "scale" = scale_result,
    "shift" = shift_result,
    "permute" = perm_result
  )
  expect_equal(result, expected = expected)


  ####
  #### Test with multiple patches of different types.
  ####
  N <- 4
  corruption <- list(purrr::partial(sample_patch_scale, mean = 100),
                     purrr::partial(sample_patch_shift, mean = -40),
                     purrr::partial(sample_patch_permute, n = 2L),
                     purrr::partial(sample_patch_shift, mean = 10))

  seed <- 12121212
  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)

  experiment <- execute_synthetic_experiment(config)

  count_attr <- "count"
  result <- metric_type_precision(experiment, short = TRUE, count_attr = count_attr)

  # Four of the results contain a scale patch. So does the corruption.
  scale_result <- 1
  attr(scale_result, which = count_attr) <- 4

  # Four of the results contain a shift patch. So does the corruption.
  shift_result <- 1
  attr(shift_result, which = count_attr) <- 4

  # One of the results contain a permutation patch. So does the corruption.
  perm_result <- 1
  attr(perm_result, which = count_attr) <- 1

  expected <- list(
    "scale" = scale_result,
    "shift" = shift_result,
    "permute" = perm_result
  )
  expect_equal(result, expected = expected)
})
