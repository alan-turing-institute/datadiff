require(testthat)
context("metric_column_accuracy function")

test_that("the metric_column_accuracy function works", {

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

  # Test the case in which the corruption contains no relevant patch type.
  corruption <- list(purrr::partial(sample_patch_permute, n = 2L))

  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  output <- execute_synthetic_experiment(config)
  expect_equal(patch_type(output$get_corruption()), expected = "permute")
  expect_equal(metric_column_accuracy(output), expected = NA)

  # Now test with a relevant patch type.
  corruption <- list(purrr::partial(sample_patch_permute, n = 2L),
                     purrr::partial(sample_patch_scale, mean = 10))

  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  output <- execute_synthetic_experiment(config)

  # The corruption is composed of a permutation and a scale patch.
  expect_equal(patch_type(output$get_corruption()),
               expected = c("permute", "scale"))

  # The ddiff algorithm proposes an affine (shift + scale) and a permutation.
  expect_true(all(purrr::map_lgl(output$results, .f = function(x) {
    identical(patch_type(x), c("scale", "shift", "permute"))
  })))

  # The corruption contains a scaling of column v2 by a factor of 10.593.
  expect_equal(patch_type(output$get_corruption()), expected = c("permute", "scale"))
  corruption_scale <- decompose_patch(output$get_corruption())[[2]]
  expect_equal(patch_type(corruption_scale), expected = "scale")
  expect_equal(get_patch_params(corruption_scale)[["cols"]], expected = 5)

  corruption_scale_factor <- get_patch_params(corruption_scale)[["scale_factor"]]
  expect_equal(corruption_scale_factor, expected = 10.59272, tolerance = 10^(-6))

  # Check that the scaling by the corruption is as expected:
  expected <- 10.59272
  actual <- mean(output$get_corruption()(data)[["v2"]])/mean(data[["v2"]])
  expect_equal(actual, expected, tolerance = 10^(-6))

  # Other columns aren't scaled:
  sink <- sapply(paste0("v", setdiff(1:5, y = 2L)), FUN = function(h) {
    expect_equal(mean(output$get_corruption()(data)[[h]])/mean(data[[h]]), 1)
  })

  # The two results both contain scalings, but only the first one correctly
  # identifies which column was scaled.

  # Check that the scaling by result1 is as expected:
  # i.e. column v2 is scaled (correctly) but the scale factor is not identified precisely.
  actual <- mean(output$get_results()[[1]](data)[["v2"]])/mean(data[["v2"]])
  expect_equal(actual, expected, tolerance = 1)

  # Other columns aren't scaled:
  sink <- sapply(paste0("v", setdiff(1:5, y = 2L)), FUN = function(h) {
    expect_equal(mean(output$get_results()[[1]](data)[[h]])/mean(data[[h]]), 1)
  })

  # Check that the scaling by result2 is as expected:
  # i.e. column v1 is scaled (erroneously) by 12.219 and shifted by -0.222.
  actual <- mean(output$get_results()[[2]](data)[["v1"]] + 0.222)/mean(data[["v1"]])
  expect_equal(actual, expected = 12.219, tolerance = 10^(-2))

  # Other columns aren't scaled:
  sink <- sapply(paste0("v", setdiff(1:5, y = 1L)), FUN = function(h) {
    expect_equal(mean(output$get_results()[[2]](data)[[h]])/mean(data[[h]]), 1)
  })

  # Hence, the reported column accuracy is 50%.
  expect_equal(metric_column_accuracy(output), expected = c("scale" = 0.5))

  # Test the 'partial' argument. In this case the result is unchanged, since
  # result1 is exactly correct and result2 is completely incorrect.
  expect_equal(metric_column_accuracy(output, partial = TRUE),
               expected = c("scale" = 0.5))

  ####
  #### Now test with an insert patch type: permute then insert.
  ####
  seed <- 22
  corruption <- list(purrr::partial(sample_patch_permute, n = 2L),
                     sample_patch_insert)

  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  output <- execute_synthetic_experiment(config)

  # The corruption inserts a patch after column 5, so its final position is 6.
  corruption_insert <- decompose_patch(output$get_corruption())[[2]]
  expect_identical(patch_type(corruption_insert, short = TRUE), expected = "insert")
  expect_equal(get_patch_params(corruption_insert)[["insertion_point"]], 5L)

  # The ddiff algorithm proposes an insert *and then* a permutation.
  expect_true(all(purrr::map_lgl(output$results, .f = function(x) {
    identical(patch_type(x), c("insert", "permute"))
  })))

  result_insert <- purrr::map(output$get_results(), .f = function(p) {
    decompose_patch(p)[[1]]
  })
  result_permute <- purrr::map(output$get_results(), .f = function(p) {
    decompose_patch(p)[[2]]
  })

  # The first result inserts after column 5 and then permutes, but leaves the
  # inserted column in position 6.
  # head(output$get_results()[[1]](data[1:250, ]))
  expect_identical(patch_type(result_insert[[1]], short = TRUE), expected = "insert")
  expect_equal(get_patch_params(result_insert[[1]])[["insertion_point"]], 5L)
  expect_equal(terminal_column_position(6L, patch = result_permute[[1]], initial = FALSE), 6L)

  # Therefore the pairwise column accuracy is 1 for this result.
  pairwise_result <- pairwise_column_accuracy(output$get_corruption(),
                                              result = output$get_results()[[1]],
                                              type = "insert", column_param = "insertion_point")
  expect_equal(pairwise_result, expected = 1)

  # The second result is identical to the first.
  pairwise_result <- pairwise_column_accuracy(output$get_corruption(),
                                              result = output$get_results()[[2]],
                                              type = "insert", column_param = "insertion_point")
  expect_equal(pairwise_result, expected = 1)

  # Finally, the metric column accuracy is the average of the two results.
  expect_equal(metric_column_accuracy(output, column_param = "insertion_point"),
               expected = c("insert" = 1))
  expect_equal(metric_column_accuracy(output, column_param = c("cols", "insertion_point")),
               expected = c("insert" = 1))

  ####
  #### Now test with an insert patch type: insert, shift then permute.
  ####
  datadiff <- purrr::partial(ddiff,
                             patch_generators = list(gen_patch_affine, gen_patch_recode),
                             patch_penalties = c(12, 12),
                             permute_penalty = 12)

  corruption <- list(sample_patch_insert,
                     purrr::partial(sample_patch_shift, mean=10),
                     purrr::partial(sample_patch_permute, n = 2L))

  config <- configure_synthetic_experiment(data, corruption = corruption,
                                           datadiff = datadiff, N = N,
                                           split = split, seed = seed)
  output <- execute_synthetic_experiment(config)

  # The corruption inserts a patch after column 1 and then leaves it in position
  # 2, so that is its final position.
  corruption_insert <- decompose_patch(output$get_corruption())[[1]]
  expect_identical(patch_type(corruption_insert, short = TRUE), expected = "insert")
  expect_equal(get_patch_params(corruption_insert)[["insertion_point"]], 1L)

  # The ddiff algorithm proposes only an insert.
  expect_true(all(purrr::map_lgl(output$get_results(), .f = function(x) {
    identical(patch_type(x), c("insert"))
  })))

  result_insert <- purrr::map(output$get_results(), .f = function(p) {
    decompose_patch(p)[[1]]
  })

  # The first result inserts after column 3.
  # head(output$get_results()[[1]](data[1:250, ]))
  expect_identical(patch_type(result_insert[[1]], short = TRUE), expected = "insert")
  expect_equal(get_patch_params(result_insert[[1]])[["insertion_point"]], 3L)

  # Therefore the pairwise column accuracy is zero for this result.
  pairwise_result <- pairwise_column_accuracy(output$get_corruption(),
                                              result = output$get_results()[[1]],
                                              type = "insert", column_param = "insertion_point")
  expect_equal(pairwise_result, expected = 0)

  # The second result is identical.
  pairwise_result <- pairwise_column_accuracy(output$get_corruption(),
                                              result = output$get_results()[[2]],
                                              type = "insert", column_param = "insertion_point")

  # Finally, the metric column accuracy contains results for all relevant patch
  # types in the corruption (given the column_param argument).
  expect_equal(metric_column_accuracy(output, column_param = "insertion_point"),
               expected = c("insert" = 0))
  expect_equal(metric_column_accuracy(output, column_param = c("cols", "insertion_point")),
               expected = c("shift" = NA, "insert" = 0))

  #### Test the vectorised column_param argument.
  expect_equal(metric_column_accuracy(output, column_param = c("abc", "insertion_point")),
               expected = c("insert" = 0))

  expect_equal(metric_column_accuracy(output, column_param = c("abc", "xzy")),
               expected = NA)

})


