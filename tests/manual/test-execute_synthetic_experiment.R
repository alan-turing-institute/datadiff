require(testthat)
context("synthetic_experiment function")

test_that("the execute_synthetic_experiment function works", {

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

  seed <- 147
  set.seed(seed) # Set the seed before generating the data!
  data <- generate_normal_df(500)
  N <- 2
  split <- 0.5
  datadiff <- purrr::partial(ddiff,
                             patch_generators = list(gen_patch_affine, gen_patch_recode))

  config <- configure_synthetic_experiment(data, N = N, datadiff = datadiff, seed = seed)
  result <- execute_synthetic_experiment(config)

  expected <- c("get_corruption",
                "get_data",
                "strip_data",
                "split",
                "datadiff",
                "N",
                "seed",
                "data_id",
                "results",
                "execution_time")

  result$strip_data()
  expect_true(all(expected %in% names(result)))
  expect_false("data" %in% names(result))

  expect_true(is.list(result$results))
  expect_equal(length(result$results), expected = N)

  sink <- sapply(result$results, FUN = function(x) {
    expect_true(is_patch(x))
  })

  # The corruptions in the experiment config & output are equal.
  expect_equal(config$get_corruption(), result$get_corruption())

  #### Test with a non-trivial corruption.
  seed <- 22
  set.seed(seed)
  data <- generate_normal_df(500)
  N <- 2
  split <- 0.5

  # Corruption: rescale then permute.
  corruption <- list(purrr::partial(sample_patch_permute, n = 2L),
                     purrr::partial(sample_patch_rescale, mean_scale = 10))

  # To get rescale patches in the result (as opposed to composed scale & shift
  # patches) we must specify the patch_generators in ddiff.
  datadiff <- purrr::partial(ddiff, patch_generators =
                               list(gen_patch_rescale, gen_patch_recode))

  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  result <- execute_synthetic_experiment(config)

  # The corruptions in the experiment config & output are equal.
  expect_equal(config$get_corruption(), result$get_corruption())

  # Check the results do not change if we repeatedly execute the experiment.
  r1 <- result$get_results()[[1]]
  r2 <- result$get_results()[[2]]

  result <- execute_synthetic_experiment(config)

  expect_equal(config$get_corruption(), result$get_corruption())

  expect_equal(result$get_results()[[1]], r1)
  expect_equal(result$get_results()[[2]], r2)

  #### Test with a rescale (atomic shift + scale) corruption.
  corruption <- list(purrr::partial(sample_patch_permute, n = 2L),
                     purrr::partial(sample_patch_rescale, mean_scale = 10,
                                    mean_shift = 4))

  # To get rescale patches in the result (as opposed to composed scale & shift
  # patches) we must specify the patch_generators in ddiff.
  datadiff <- purrr::partial(ddiff, patch_generators =
                               list(gen_patch_rescale, gen_patch_recode))

  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  result <- execute_synthetic_experiment(config)

  # The corruptions in the experiment config & output are equal.
  expect_equal(config$get_corruption(), result$get_corruption())

  # Check the results do not change if we repeatedly execute the experiment.
  r1 <- result$get_results()[[1]]
  r2 <- result$get_results()[[2]]

  result <- execute_synthetic_experiment(config)

  expect_equal(config$get_corruption(), result$get_corruption())

  expect_equal(result$get_results()[[1]], r1)
  expect_equal(result$get_results()[[2]], r2)

  ####
  #### Test with the UCI abalone dataset.
  ####
  seed <- 147
  data_id <- "abalone"
  datadiff <- purrr::partial(ddiff,
                             patch_generators = list(gen_patch_affine, gen_patch_recode),
                             patch_penalties = c(5/2, 5/2),
                             permute_penalty = 0)
  split <- 0.5

  #### Test with multiple corruption types.
  N <- 2
  corruption <- list(purrr::partial(sample_patch_permute, n = 2L),
                     sample_patch_rescale)

  config <- configure_synthetic_experiment(data_id, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  result <- execute_synthetic_experiment(config)
  expect_equal(length(result$results), expected = N)

  #### Test with a sample scale patch.

  # Note that this is a repetition of *part of* the preceding experimement (i.e.
  # with a subset of corruptions) but with the same random seed. The results
  # for the scale patch are *not* identical because the scale corruptions are not
  # identical. This is deliberate. If they were identical then it would not be
  # possible to include the same sample corruption type multiple times in the
  # same synthetic experiment (as they would all give exactly the same corruption),
  # which is something we want to be able to do.
  N <- 20
  corruption <- list(purrr::partial(sample_patch_scale, sd = 1))

  config <- configure_synthetic_experiment(data_id, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  result <- execute_synthetic_experiment(config)
  expect_equal(length(result$results), expected = N)

  # With N = 20 we get false negatives (on runs 18 & 19).
  expect_false(all(purrr::map_lgl(result$results[1:17], is_identity_patch)))
  expect_true(all(purrr::map_lgl(result$results[18:19], is_identity_patch)))

  #### Test with a sample insert patch.
  N <- 2
  corruption <- list(sample_patch_insert)

  config <- configure_synthetic_experiment(data_id, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  result <- execute_synthetic_experiment(config)
  expect_equal(length(result$results), expected = N)

  #### Test with a sample delete patch.
  N <- 2
  corruption <- list(sample_patch_delete)

  config <- configure_synthetic_experiment(data_id, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  result <- execute_synthetic_experiment(config)
  expect_equal(length(result$results), expected = N)

  #### Test with a sample delete patch.
  N <- 2
  corruption <- list(sample_patch_break)

  config <- configure_synthetic_experiment(data_id, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  result <- execute_synthetic_experiment(config)
  expect_equal(length(result$results), expected = N)

  #### Test with a sample delete-and-then-permute patch.

  # When the corruption is a composed patch the get_corruption function must
  # take into account the patches that have already been applied.
  N <- 2
  corruption <- list(sample_patch_delete, sample_patch_permute)

  config <- configure_synthetic_experiment(data_id, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  result <- execute_synthetic_experiment(config)
  expect_equal(length(result$results), expected = N)

  ####
  #### Test with the data.gov.uk broadband dataset.
  ####
  seed <- 14715
  data_id <- "broadband2013"
  datadiff <- purrr::partial(ddiff,
                             patch_generators = list(gen_patch_affine, gen_patch_recode),
                             permute_penalty = 0)
  split <- 0.5

  #### Test with multiple corruption types.
  N <- 2
  corruption <- list(purrr::partial(sample_patch_permute, n = 2L),
                     sample_patch_rescale)

  config <- configure_synthetic_experiment(data_id, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  result <- execute_synthetic_experiment(config)
  expect_true(executed(result))
  expect_equal(length(result$results), expected = N)

})
