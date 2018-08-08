require(testthat)
context("read_data")

test_that("the read_data function works", {

  ##
  ## Source: UCI Machine Learning Repository
  ##
  source <- "UCI"

  dataset <- "xyz"
  expect_error(read_data(dataset, source = source), regexp = "is not TRUE")

  dataset <- "iris"

  result <- read_data(dataset, source = source)

  expect_true(is.data.frame(result))
  iris_colnames <- c("sepal_length", "sepal_width", "petal_length",
                     "petal_width", "class")
  expect_equal(colnames(result), expected = iris_colnames)
  expect_true(nrow(result) > 0)

  # Test with a vector 'dataset' argument.
  dataset <- datasets(source)
  result <- read_data(dataset, source = source)

  expect_true(is.list(result))
  expect_false(is.data.frame(result))
  expect_identical(names(result), expected = dataset)

  sink <- sapply(dataset, FUN = function(d) {
    expect_true(is.data.frame(result[[d]]))
    expect_true(nrow(result[[d]]) > 0)
  })

  ##
  ## Source: data.gov.uk
  ##
  source <- "data.gov.uk"

  dataset <- "xyz"
  expect_error(read_data(dataset, source = source), regexp = "is not TRUE")

  dataset <- "broadband"
  result <- read_data(dataset, source = source, year = 2013)
  expect_true(is.data.frame(result))

  result <- read_data(dataset, source = source, year = 2014)
  expect_true(is.data.frame(result))

  result <- read_data(dataset, source = source, year = 2015)
  expect_true(is.data.frame(result))

  result <- read_data(dataset, source = source, year = 2016)
  expect_true(is.data.frame(result))

  expect_error(read_data(dataset, source = source, 2100), regexp = "is not TRUE")
})
