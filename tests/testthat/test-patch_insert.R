require(testthat)
context("patch_insert")

test_that("the patch_insert constructor and get_patch_params functions work", {

  target <- patch_insert(2L, data = data.frame(rep(NA, 10)))

  expect_true(is(target, "patch"))
  expect_true(is(target, "patch_insert"))
  expected <- list("insertion_point"=2L, "data"=data.frame(rep(NA, 10)))
  expect_equal(get_patch_params(target), expected)

  expect_error(patch_insert(c(2L, 5L), data = data.frame(rep(NA, 10))))
})

test_that("patch function application works", {

  ## Delete by column index.
  df <- data.frame(col1 = 1:10, col2 = 11:20)
  expect_equal(length(df), expected = 2)

  data <- data.frame("x" = rep(NA, 10))

  target <- patch_insert(0L, data = data)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(length(result), expected = 3)
  expect_identical(names(result), c(names(data), names(df)))
  expect_identical(result[1], data)
  expect_identical(result[2:3], df)

  target <- patch_insert(1L, data = data)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(length(result), expected = 3)
  expect_identical(names(result), c(names(df)[1], names(data), names(df)[2]))
  expect_identical(result[2], data)
  expect_identical(result[c(1,3)], df)

  target <- patch_insert(2L, data = data)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(length(result), expected = 3)
  expect_identical(names(result), c(names(df), names(data)))
  expect_identical(result[3], data)
  expect_identical(result[1:2], df)

  target <- patch_insert(3L, data = data)
  expect_error(target(df), regexp = "is_compatible_columns")

  # Test with a character insertion point and multi-column data.
  data <- data.frame("x" = rep(NA, 10), "y" = 10:1)

  target <- patch_insert("col1", data = data)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(length(result), expected = 4)
  expect_identical(names(result), c("col1", names(data), "col2"))
  expect_identical(result[2:3], data)
  expect_identical(result[c(1, 4)], df)

  target <- patch_insert("col2", data = data)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(length(result), expected = 4)
  expect_identical(names(result), c(names(df), names(data)))
  expect_identical(result[3:4], data)
  expect_identical(result[1:2], df)

  # Test with conflicting column names.
  # Note the use of tibble::repair_names (which is indirectly used by
  # patch_insert as a result of the call to dplyr::bind_cols).
  data <- data.frame("x" = rep(NA, 10), "col2" = 10:1)

  target <- patch_insert("col1", data = data)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(length(result), expected = 4)
  expect_identical(names(result),
                   names(tibble::repair_names(cbind(df[1], data, df[2]))))
  expect_identical(result[2:3], data)
  expect_identical(result[c(1, 4)],
                   tibble::repair_names(cbind(df[1], data, df[2]))[c(1, 4)])

  data <- data.frame("x" = rep(NA, 10), "col2" = 10:1)

  target <- patch_insert("col2", data = data)
  result <- target(df)

  expect_true(is.data.frame(result))
  expect_equal(length(result), expected = 4)
  expect_identical(names(result),
                   names(tibble::repair_names(cbind(df, data))))
  expect_identical(result[3:4],
                   tibble::repair_names(cbind(df, data))[3:4])
  expect_identical(result[1:2], df)

  # Apply to an incompatible data frame.
  df <- data.frame(col1 = 1:10, col2 = 11:20)
  data <- data.frame("x" = rep(NA, 10), "y" = 10:1)

  target <- patch_insert(1L, data = data[1:9, ])
  expect_error(target(df), regexp = "nrow")

  target <- patch_insert("c", data = data)
  expect_error(target(df), regexp = "is_compatible_columns.*not TRUE")
})

test_that("the sample_patch_insert function works", {

  df <- mtcars
  colname <- "INSERTED"
  result <- sample_patch_insert(df, colname = colname)
  expect_true(is_patch(result))
  expect_equal(patch_type(result), "insert")
  expect_equal(ncol(result(df)), ncol(df) + 1)
  expect_true(setequal(colnames(result(df)), c(colnames(df), colname)))

  # Test inserting a second column.
  df <- result(df)
  result <- sample_patch_insert(df, colname = colname)
  expect_true(is_patch(result))
  expect_equal(patch_type(result), "insert")
  expect_equal(ncol(result(df)), ncol(df) + 1)
  # This time the column name has the suffix "1".
  expect_true(setequal(colnames(result(df)), c(colnames(df), paste0(colname, "1"))))

  # Test with parameters passed to rdist.
  df <- mtcars
  set.seed(147)
  result <- sample_patch_insert(df, mean = 10, sd = 4)
  expect_true(is_patch(result))
  expect_equal(patch_type(result), "insert")
  expect_equal(mean(get_patch_params(result)[["data"]][[colname]]), 10,
               tolerance = 0.16)

  # Test with a discrete distribution for the data.
  rdist <- function(n, ...) { sample(letters[1:3], size = n, ...) }
  expect_error(sample_patch_insert(df, rdist = rdist),
               regexp = "cannot take a sample larger than the population")

  result <- sample_patch_insert(df, rdist = rdist, replace = TRUE)
  expect_true(is_patch(result))
  expect_equal(patch_type(result), "insert")
  expect_true(setequal(unique(get_patch_params(result)[["data"]][[colname]]),
                       letters[1:3]))

})
