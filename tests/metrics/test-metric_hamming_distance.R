require(testthat)
context("metric_hamming_distance function")

test_that("the metric_hamming_distance function works", {

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

  # The corruption is composed of a permutation and a scale patch.
  expect_equal(patch_type(experiment$get_corruption()),
               expected = c("permute", "scale"))

  # The ddiff algorithm proposes an affine (shift + scale) and a permutation.
  expect_true(all(purrr::map_lgl(experiment$results, .f = function(x) {
    identical(patch_type(x), c("scale", "shift", "permute"))
  })))

  # The corruption contains a permutation.
  expect_equal(patch_type(experiment$get_corruption()), expected = c("permute", "scale"))
  corruption_perm <- decompose_patch(experiment$get_corruption())[[1]]
  expect_equal(patch_type(corruption_perm), expected = "permute")
  expect_equal(order(get_patch_params(corruption_perm)[["perm"]]),
               expected = c(1L, 5L, 3L, 4L, 2L))

  # The two results both contain permutations.

  # Result one matches the corruption permutation perfectly...
  result1_perm <- decompose_patch(experiment$get_results()[[1]])[[3]]
  expect_equal(order(get_patch_params(result1_perm)[["perm"]]),
               expected = c(1L, 5L, 3L, 4L, 2L))

  # ...result two does not.
  result2_perm <- decompose_patch(experiment$get_results()[[2]])[[3]]
  expect_equal(order(get_patch_params(result2_perm)[["perm"]]),
               expected = c(5L, 1L, 3L, 4L, 2L))

  # The average Hamming distance is (0 + 2)/2 = 1, calculated by averaging over
  # two terms.
  expected <- c("permute" = 1)
  attr(expected, which = "count") <- 2L
  expect_equal(metric_hamming_distance(experiment), expected = expected)

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

  experiment <- execute_synthetic_experiment(config)

  # The corruption is composed of a permutation and a scale patch.
  expect_equal(patch_type(experiment$get_corruption()),
               expected = c("scale", "permute", "scale"))

  # The ddiff algorithm proposes a permutation in every result.
  expect_true(all(purrr::map_lgl(experiment$get_results(), .f = function(x) {
    "permute" %in% patch_type(x)
  })))

  # The corruption contains a permutation in position 2.
  corruption_perm <- decompose_patch(experiment$get_corruption())[[2]]
  expect_equal(patch_type(corruption_perm), expected = "permute")
  expect_equal(order(get_patch_params(corruption_perm)[["perm"]]),
               expected = c(1L, 4L, 3L, 2L, 5L))

  # The four results all contain permutations.

  # The Hamming distance to the permutation in result 1 is 3.
  result1_perm <- decompose_patch(experiment$get_results()[[1]])[[5]]
  expect_equal(order(get_patch_params(result1_perm)[["perm"]]),
               expected = c(1L, 2L, 4L, 3L, 5L))

  # The Hamming distance to the permutation in result 2 is 0.
  result2_perm <- decompose_patch(experiment$get_results()[[2]])[[3]]
  expect_equal(order(get_patch_params(result2_perm)[["perm"]]),
               expected = c(1L, 4L, 3L, 2L, 5L))

  # The Hamming distance to the permutation in result 3 is 0.
  result3_perm <- decompose_patch(experiment$get_results()[[3]])[[5]]
  expect_equal(order(get_patch_params(result3_perm)[["perm"]]),
               expected = c(1L, 4L, 3L, 2L, 5L))

  # The Hamming distance to the permutation in result 4 is 0.
  result4_perm <- decompose_patch(experiment$get_results()[[4]])[[5]]
  expect_equal(order(get_patch_params(result4_perm)[["perm"]]),
               expected = c(1L, 4L, 3L, 2L, 5L))

  # The average Hamming distance is (3 + 0 + 0 + 0)/4 = 3/4.
  expected <- c("permute" = 3/4)
  attr(expected, which = "count") <- 4
  expect_equal(metric_hamming_distance(experiment), expected = expected)
})
