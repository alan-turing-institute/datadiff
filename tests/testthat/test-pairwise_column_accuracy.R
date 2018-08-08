require(testthat)
context("pairwise_column_accuracy function")

test_that("the pairwise_column_accuracy function works", {

  # Test with a recode.
  target <- patch_recode(cols=2L, encoding = c("a" = 1))

  result <- patch_recode(cols = 1L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 0)

  # The 2nd column is recoded.
  result <- patch_recode(cols = 2L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 1)

  result <- patch_recode(cols = 3L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 0)

  result <- patch_recode(cols = 4L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 0)

  expect_error(pairwise_column_accuracy(target, result = result, type = "shift"),
               regexp = "given patch does not contain any components of type")

  #### Bugfix 10/11/2017:
  # Test with type = "insert" for the case of an insert patch.
  target <- patch_insert(insertion_point = 0L, data = data.frame(NA))

  result <- patch_insert(insertion_point = 0L, data = data.frame(NA))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "insert",
                                        column_param = "insertion_point"),
               expected = 1)

  result <- patch_insert(insertion_point = 1L, data = data.frame(NA))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "insert",
                                        column_param = "insertion_point"),
               expected = 0)

  target <- patch_insert(insertion_point = 1L, data = data.frame(NA))

  result <- patch_insert(insertion_point = 1L, data = data.frame(NA))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "insert",
                                        column_param = "insertion_point"),
               expected = 1)

  result <- patch_insert(insertion_point = 0L, data = data.frame(NA))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "insert",
                                        column_param = "insertion_point"),
               expected = 0)

  result <- patch_insert(insertion_point = 2L, data = data.frame(NA))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "insert",
                                        column_param = "insertion_point"),
               expected = 0)

  # Test with type = "insert" for the case of an insert + permute patch.
  target <- compose_patch(patch_permute(c(2L, 1L, 3L)),
                          patch_insert(insertion_point = 0L, data = data.frame(NA)))

  result <- patch_insert(insertion_point = 0L, data = data.frame(NA))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "insert",
                                        column_param = "insertion_point"),
               expected = 0)

  result <- patch_insert(insertion_point = 1L, data = data.frame(NA))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "insert",
                                        column_param = "insertion_point"),
               expected = 1)

  result <- patch_insert(insertion_point = 2L, data = data.frame(NA))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "insert",
                                        column_param = "insertion_point"),
               expected = 0)

  # Another test with type = "insert" for the case of an insert + permute patch.
  target <- compose_patch(patch_permute(c(3L, 2L, 1L)),
                          patch_insert(insertion_point = 2L, data = data.frame(NA)))

  result <- patch_insert(insertion_point = 0L, data = data.frame(NA))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "insert",
                                        column_param = "insertion_point"),
               expected = 1)

  result <- patch_insert(insertion_point = 1L, data = data.frame(NA))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "insert",
                                        column_param = "insertion_point"),
               expected = 0)

  result <- patch_insert(insertion_point = 2L, data = data.frame(NA))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "insert",
                                        column_param = "insertion_point"),
               expected = 0)

  #### Bugfix 27/09/2017:
  # Test the case of failed patch type recall (i.e. the target contains a
  # columnwise patch but the result does not). In this case the result should be
  # NA (*not* 0).
  target <- patch_shift(cols = 3L, shift = 2)
  result <- patch_identity()
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = NA)

  # Test with a permutation followed by a recode.
  target <- compose_patch(patch_recode(cols=2L, encoding = c("a" = 1)),
                          patch_permute(c(1L, 4L, 3L, 2L)))

  result <- patch_recode(cols = 1L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 0)

  result <- patch_recode(cols = 2L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 0)

  result <- patch_recode(cols = 3L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 0)

  # The 4th column in the original data is shifted.
  result <- patch_recode(cols = 4L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 1)

  # Test with a recode followed by a permutation.
  target <- compose_patch(patch_permute(c(1L, 4L, 3L, 2L)),
                          patch_recode(cols=2L, encoding = c("a" = 1)))

  result <- patch_recode(cols = 1L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 0)

  # The 2nd column in the original data is shifted.
  result <- patch_recode(cols = 2L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 1)

  result <- patch_recode(cols = 3L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 0)

  result <- patch_recode(cols = 4L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 0)

  # Test with a shift followed by an insert and a permutation.
  target <- compose_patch(patch_permute(c(1L, 4L, 5L, 3L, 2L)),
                          patch_insert(insertion_point = 0L, data = data.frame()),
                          patch_shift(cols=4L, shift = 1))

  result <- patch_shift(cols = 1L, shift = 2)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 0)

  result <- patch_shift(cols = 2L, shift = 2)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 0)

  result <- patch_shift(cols = 3L, shift = 2)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 0)

  # The 4th column in the original data is shifted.
  result <- patch_shift(cols = 4L, shift = 2)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 1)

  result <- patch_shift(cols = 5L, shift = 2)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 0)

  # Test with a permutation followed by a shift and then an insert.
  target <- compose_patch(patch_insert(insertion_point = 0L, data = data.frame()),
                          patch_shift(cols=2L, shift = 1),
                          patch_permute(c(1L, 4L, 3L, 2L)))

  result <- patch_shift(cols = 1L, shift = 2)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 0)

  result <- patch_shift(cols = 2L, shift = 2)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 0)

  result <- patch_shift(cols = 3L, shift = 2)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 0)

  # The 4th column in the original data is shifted.
  result <- patch_shift(cols = 4L, shift = 2)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 1)

  result <- patch_shift(cols = 5L, shift = 2)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 0)

  #### Test with multiple transformed columns.

  target <- compose_patch(patch_recode(cols=2L, encoding = c("a" = 1)),
                          patch_recode(cols=4L, encoding = c("a" = 1)))

  result <- patch_recode(cols = 1L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 0)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode", partial = TRUE),
               expected = 0)

  result <- patch_recode(cols = 2L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 0)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode", partial = TRUE),
               expected = 1/2)

  result <- patch_recode(cols = 3L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 0)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode", partial = TRUE),
               expected = 0)

  result <- patch_recode(cols = 4L, encoding = c("b" = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 0)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode", partial = TRUE),
               expected = 1/2)

  # The 2nd & 4th columns are shifted.
  result <- compose_patch(patch_recode(cols=2L, encoding = c("b" = 2)),
                          patch_recode(cols=4L, encoding = c("b" = 2)))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode"),
               expected = 1)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "recode", partial = TRUE),
               expected = 1)

  # Test with a permutation followed by a shift, then an insert & another shift.
  target <- compose_patch(patch_shift(cols=2L, shift = 1),
                          patch_insert(insertion_point = 0L, data = data.frame()),
                          patch_shift(cols=2L, shift = 1),
                          patch_permute(c(1L, 4L, 3L, 2L)))

  result <- patch_shift(cols = 1L, shift = 1)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 0)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift", partial = TRUE),
               expected = 1/2)

  result <- patch_shift(cols = 2L, shift = 1)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 0)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift", partial = TRUE),
               expected = 0)

  result <- patch_shift(cols = 3L, shift = 1)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 0)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift", partial = TRUE),
               expected = 0)

  result <- patch_shift(cols = 4L, shift = 1)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 0)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift", partial = TRUE),
               expected = 1/2)

  result <- patch_shift(cols = 5L, shift = 1)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 0)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift", partial = TRUE),
               expected = 0)

  # The 4th & 1st columns are shifted.
  result <- compose_patch(patch_shift(cols = 1L, shift = 2),
                          patch_shift(cols = 4L, shift = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 1)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift", partial = TRUE),
               expected = 1)

  # Order is unimportant.
  result <- compose_patch(patch_shift(cols = 4L, shift = 2),
                          patch_shift(cols = 1L, shift = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 1)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift", partial = TRUE),
               expected = 1)

  result <- compose_patch(patch_shift(cols = 2L, shift = 2),
                          patch_shift(cols = 2L, shift = 2))
  expect_error(pairwise_column_accuracy(target, result = result, type = "shift"),
               regexp = "is_columnwise_unique")

  result <- compose_patch(patch_shift(cols = 2L, shift = 2),
                          patch_shift(cols = 4L, shift = 2))
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift"),
               expected = 0)
  expect_equal(pairwise_column_accuracy(target, result = result, type = "shift", partial = TRUE),
               expected = 1/2)

  ####
  #### Integration test with a dummy dataset.
  ####
  generate_normal_df <- function(n) {
    v1 <- rnorm(n)
    v2 <- rnorm(n, sd = 5/4)
    v3 <- rnorm(n, mean = 1/50)
    v4 <- rnorm(n, sd = 2, mean = 4)
    v5 <- rnorm(n, sd = 4, mean = 8)
    data.frame("v1" = v1, "v2" = v2, "v3" = v3, "v4" = v4, "v5" = v5)
  }

  seed <- 22
  set.seed(seed) # Set the seed before generating the data!
  data <- generate_normal_df(500)
  N <- 2
  split <- 0.5
  corruption <- list(purrr::partial(sample_patch_permute, n = 2L),
                     purrr::partial(sample_patch_scale, mean = 10))

  patch_generators <- list(gen_patch_affine, gen_patch_recode)
  patch_penalties = c(0.999999, 0.999999)
  penalty_scaling <- purrr::partial(ks_scaling, nx = 250, ny = 250)
  datadiff <- purrr::partial(ddiff, patch_generators = patch_generators,
                             patch_penalties = patch_penalties,
                             penalty_scaling = penalty_scaling)

  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  output <- execute_synthetic_experiment(config)

  # The corruption contains a scaling of column v2.
  corruption_scale_icp <- terminal_column_position(get_patch_params(decompose_patch(output$get_corruption())[[2]])[["cols"]],
                                                   patch = output$get_corruption(),
                                                   initial = TRUE)
  expect_equal(corruption_scale_icp, expected = 2L)

  # This is correctly identified in results[[2]], but not in results[[1]] (since
  # in results[[1]] _both_ columns v2 and v5 are scaled).
  expect_equal(pairwise_column_accuracy(target = output$get_corruption(),
                                        result = output$results[[1]], type = "scale"),
               expected = 0)

  expect_equal(pairwise_column_accuracy(target = output$get_corruption(),
                                        result = output$results[[1]], type = "scale",
                                        partial = TRUE),
               expected = 1/2)

  expect_equal(pairwise_column_accuracy(target = output$get_corruption(),
                                        result = output$results[[2]], type = "scale"),
               expected = 1)
  expect_equal(pairwise_column_accuracy(target = output$get_corruption(),
                                        result = output$results[[2]], type = "scale",
                                        partial = TRUE),
               expected = 1)

  # The corruption does not contain any components of type 'shift'.
  expect_error(pairwise_column_accuracy(target = output$get_corruption(),
                                        result = output$results[[1]], type = "shift"),
               regexp = "patch does not contain any components of type")

  # The 'permute' type is not valid for testing column accuracy.
  expect_error(pairwise_column_accuracy(target = output$get_corruption(),
                                        result = output$results[[1]], type = "permute"),
               regexp = "Column index parameter not found")

})

