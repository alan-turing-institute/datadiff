require(testthat)
context("fetch_data")

test_that("the fetch_data function works", {

  test_data_dir <- file.path(system.file("data", package = "datadiff"), "test")

  # Clean the 'test' storage directory. Note that this also happens whenever
  # the package is loaded.
  if (file.exists(test_data_dir))
    unlink(test_data_dir, recursive = TRUE)
  sink <- dir.create(test_data_dir)

  ##
  ## Source: UCI Machine Learning Repository
  ##
  source <- "UCI"

  # Test with an individual dataset
  dataset <- "abalone"
  path <- storage_path(source, dataset = dataset, storage_root = test_data_dir)

  expect_false(file.exists(path))
  fetch_data(source,
             dataset = dataset,
             path = storage_path(source, dataset = dataset,
                                 storage_root = test_data_dir))
  expect_true(file.exists(path))

  # # Test with the remaining datasets as a vector argument.
  # other_datasets <- setdiff(datasets(source), dataset)
  # dataset <- setdiff(datasets(source), dataset)
  #
  # sink <- sapply(dataset, FUN = function(d) {
  #   path <- storage_path(source, dataset = d, storage_root = test_data_dir)
  #   expect_false(file.exists(path))
  #   fetch_data(source, dataset = d, path = path)
  #   expect_true(file.exists(path))
  # })

  # Cleanup.
  unlink(test_data_dir, recursive = TRUE)

  ##
  ## Source: data.gov.uk
  ##
  source <- "data.gov.uk"
  dataset <- "broadband"
  path <- storage_path(source, dataset = dataset, storage_root = test_data_dir)

  year = 2014
  expect_false(file.exists(path))
  fetch_data(source,
             dataset = dataset,
             path = storage_path(source, dataset = dataset,
                                 storage_root = test_data_dir),
             year = year)
  expect_true(file.exists(path))

  year = 2016
  expect_false(file.exists(path))
  fetch_data(source,
             dataset = dataset,
             path = storage_path(source, dataset = dataset,
                                 storage_root = test_data_dir),
             year = year)
  expect_true(file.exists(path))

})
