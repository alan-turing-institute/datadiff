require(testthat)
context("synthetic_experiment function")

test_that("the execute_synthetic_experiment function works", {

  ##
  ## Test with execute = TRUE
  ##
  execute <- TRUE

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
  data_ids <- list(generate_normal_df(500))
  datadiff <- purrr::partial(ddiff,
                             patch_generators = list(gen_patch_affine, gen_patch_recode))
  split <- 0.5
  N <- 2
  corruptions <- list(sample_patch_identity,
                      sample_patch_recode)

  result <- batch_experiment(data_ids, corruptions = corruptions,
                             datadiff = datadiff, N = N,
                             seed = seed, execute = execute)

  expect_true(is.list(result))
  expect_equal(length(result), expected = 1) # One data frame
  expect_equal(length(result[[1]]), expected = 2) # Two corruptions

  # The result is NA for the second corruption because there are no categorical
  # columns so the recode patch is not applicable.
  expect_true(is_synthetic_experiment(result[[1]][[1]]))
  expect_true(executed(result[[1]][[1]]))
  expect_false(executed(result[[1]][[2]]))

  # The data are stripped from the result. (This avoids bloating when the
  # data_ids are character vectors rather than data frames.)
  expect_false("data" %in% names(result[[1]][[1]]))

  # Test with three UCI datasets.
  seed <- 147
  data_ids <- c("abalone", "iris", "heartdisease")
  datadiff <- purrr::partial(ddiff, permute_penalty = 0)
  split <- 0.5
  N <- 2

  sample_patch_permute2 <- purrr::partial(sample_patch_permute, n = 2L)
  rdist_scale2 <- purrr::partial(stats::rnorm, mean = 2)
  sample_patch_rescale2 <- purrr::partial(sample_patch_rescale,
                                         rdist_scale = rdist_scale2)

  corruptions <- list(sample_patch_identity,
                      sample_patch_permute2,
                      sample_patch_rescale2,
                      sample_patch_recode,
                      sample_patch_insert,
                      sample_patch_delete,
                      list(sample_patch_permute2, sample_patch_rescale2),
                      list(sample_patch_delete, sample_patch_permute2),
                      list(sample_patch_permute2, sample_patch_insert)
  )

  result <- batch_experiment(data_ids, corruptions = corruptions,
                             datadiff = datadiff, N = N,
                             seed = seed, execute = execute)

  expect_true(is.list(result))
  expect_equal(length(result), expected = 3) # Three data frames
  expect_identical(names(result), expected = data_ids)
  expect_true(all(purrr::map_lgl(data_ids, .f = function(data_id) {
    length(result[[data_id]]) == length(corruptions)
  })))

})
