require(testthat)
context("configure_synthetic_experiment function")

test_that("the configure_synthetic_experiment function works", {

  result <- configure_synthetic_experiment(mtcars)

  # The result is an R environment whose parent environment is the datadiff
  # namespace.
  expect_true(is.environment(result))
  expect_identical(parent.env(result), expected = asNamespace("datadiff"))
  expect_true(is_synthetic_experiment(result))

  expect_false(executed(result))

  expected <- c("get_corruption",
                "get_data",
                "strip_data",
                "split",
                "datadiff",
                "N",
                "seed",
                "data_id")

  expect_true(all(expected %in% names(result)))

  # Test that the data are not read when the configuration object is
  # constructed, only when get_data is called for the first time.
  expect_false("data" %in% names(result))
  sink <- result$get_data()
  expect_true("data" %in% names(result))

  # Test the strip_data function.
  result$strip_data()
  expect_false("data" %in% names(result))

  # Check that get_data still works.
  sink <- result$get_data()
  expect_true("data" %in% names(result))

  # Check the corruptions element. By default, the only 'corruption' is the
  # identity patch (i.e. no corruption.).
  p <- result$get_corruption()
  expect_true(is_patch(p))
  expect_true(is_identity_patch(p))

  #### Test the corruption argument.

  # Test that the same sequence of corruptions is generated for the same random seed.
  corruption <- list(sample_patch_permute, sample_patch_rescale)
  result <- configure_synthetic_experiment(mtcars, corruption = corruption, seed = 147)

  p <- result$get_corruption()
  expect_true(is_patch(p, allow_composed = TRUE))
  expect_false(is_patch(p, allow_composed = FALSE))

  corruptions <- decompose_patch(p)
  expected1 <- get_patch_params(corruptions[[1]])
  expected2 <- get_patch_params(corruptions[[2]])

  # Every call to get_corruptions returns the same patch.
  q <- result$get_corruption()
  corruptions <- decompose_patch(q)
  expect_true(identical(get_patch_params(corruptions[[1]]), expected1))
  expect_true(identical(get_patch_params(corruptions[[2]]), expected2))

  result <- configure_synthetic_experiment(mtcars, corruption = corruption, seed = 147)
  r <- result$get_corruption()
  corruptions <- decompose_patch(r)
  expect_true(identical(get_patch_params(corruptions[[1]]), expected1))
  expect_true(identical(get_patch_params(corruptions[[2]]), expected2))

  # If the random seed is different, so are the parameters.
  result <- configure_synthetic_experiment(mtcars, corruption = corruption, seed = 22)
  s <- result$get_corruption()
  corruptions <- decompose_patch(s)
  expect_false(identical(get_patch_params(corruptions[[1]]), expected1))
  expect_false(identical(get_patch_params(corruptions[[2]]), expected2))

  #### Test with multiple corruptions of the same type.
  corruption <- list(sample_patch_insert, sample_patch_insert)
  result <- configure_synthetic_experiment(mtcars, corruption = corruption, seed = 147)

  p <- result$get_corruption()
  corruptions <- decompose_patch(p)
  expect_true(all(purrr::map_chr(corruptions, patch_type) == "insert"))

  # Test that the parameters of the two insert corruptions are different.
  expect_false(identical(get_patch_params(corruptions[[1]])[["insertion_point"]],
                         get_patch_params(corruptions[[2]])[["insertion_point"]]))

  #### Test with a permutation corruption.
  corruption <- list(sample_patch_permute)
  result <- configure_synthetic_experiment(mtcars, corruption = corruption, seed = 22)

  # Note that getting the corruption causes the data to be read (unless it has
  # already been read).
  expect_false("data" %in% names(result))
  p <- result$get_corruption()
  expect_true(is_patch(p))
  expect_equal(patch_type(p), expected = "permute")

  # Randomly selected permutation is predictable if we know the seed.
  expected <- c(4, 5, 9, 10, 6, 8, 11, 3, 2, 1, 7)
  expect_equal(get_patch_params(p)[["perm"]], expected = expected)

  #### Test with a rescale corruption
  result <- configure_synthetic_experiment(mtcars,
                                           corruption = sample_patch_rescale,
                                           seed = 22)

  # Check the corruptions element
  p <- result$get_corruption()
  expect_true(is_patch(p))
  expect_equal(patch_type(p), expected = "rescale")

  # Randomly selected scale_factor is predictable if we know the seed.
  expect_equal(get_patch_params(p)[["scale_factor"]],
               expected = 1.051795, tolerance = 10^(-6))

  #### Test the data_reader argument using datasets saved in the package.

  data_reader <- function(data_id) {
    if (data_id == "broadband2015")
      return(broadband2015)
    if (data_id == "broadband2014")
      return(broadband2014)
    stop("Invalid data_id")
  }

  data_id <- "broadband2014"
  result <- configure_synthetic_experiment(data_id,
                                           corruption = sample_patch_permute,
                                           data_reader = data_reader,
                                           seed = 22)

  expect_true(is.environment(result))
  expect_true(is_synthetic_experiment(result))

  # Test that the data are not read when the configuration object is
  # constructed, only when get_data is called for the first time.
  expect_false("data" %in% names(result))
  sink <- result$get_data()
  expect_true("data" %in% names(result))

  # Test the strip_data function.
  result$strip_data()
  expect_false("data" %in% names(result))

  # Check that get_data still works.
  sink <- result$get_data()
  expect_true("data" %in% names(result))

  # Check the corruption.
  p <- result$get_corruption()
  expect_true(is_patch(p))
  expect_equal(patch_type(p), expected = "permute")
})
