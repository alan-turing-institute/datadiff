require(testthat)
context("batch_experiment function")

test_that("the batch_experiment function works", {

  ##
  ## Test with execute = FALSE
  ##
  execute <- FALSE

  seed <- 147
  data_ids <- c("mtcars", "airquality", "attenu")
  datadiff <- purrr::partial(ddiff, permute_penalty = 0)
  split <- 0.5
  N <- 2

  sample_patch_permute2 <- purrr::partial(sample_patch_permute, n = 2L)
  sample_patch_scale2 <- purrr::partial(sample_patch_scale, mean = 2)
  corruptions <- list(sample_patch_identity,
                      sample_patch_permute2,
                      sample_patch_scale2,
                      sample_patch_insert,
                      sample_patch_delete,
                      list(sample_patch_permute2, sample_patch_scale2),
                      list(sample_patch_delete, sample_patch_permute2),
                      list(sample_patch_permute2, sample_patch_insert)
  )

  result <- batch_experiment(data_ids, corruptions = corruptions, N = N,
                             seed = seed, execute = execute)

  expect_true(is.list(result))
  expect_identical(names(result), expected = data_ids)
  expect_true(all(purrr::map_lgl(data_ids, .f = function(data_id) {
    length(result[[data_id]]) == length(corruptions)
  })))

  expect_true(all(purrr::map_lgl(data_ids, .f = function(data_id) {
    is_identity_patch(result[[data_id]][[1]]$get_corruption()) &&
      identical(patch_type(result[[data_id]][[2]]$get_corruption()),
                "permute") &&
      identical(patch_type(result[[data_id]][[3]]$get_corruption()),
                "scale") &&
      identical(patch_type(result[[data_id]][[4]]$get_corruption()),
                "insert") &&
      identical(patch_type(result[[data_id]][[5]]$get_corruption()),
                "delete") &&
      identical(patch_type(result[[data_id]][[6]]$get_corruption()),
                c("permute", "scale")) &&
      identical(patch_type(result[[data_id]][[7]]$get_corruption()),
                c("delete", "permute")) &&
      identical(patch_type(result[[data_id]][[8]]$get_corruption()),
                c("permute", "insert"))
  })))

  # Check that each dataset is only read once.
  data_reader <- function(dataset) {
    called <<- called + 1
    get(dataset)
  }
  assign("called", value = 0, pos = environment(data_reader))

  expect_equal(get("called", pos = environment(data_reader)), expected = 0)
  execute <- FALSE
  result <- batch_experiment(data_ids, corruptions = corruptions, N = N,
                             seed = seed, execute = execute,
                             data_reader = data_reader)

  expect_equal(get("called", pos = environment(data_reader)), expected = 0)

  # Read the data using get_data in the first config object.
  expect_true(is.data.frame(result$mtcars[[1]]$get_data()))
  expect_equal(get("called", pos = environment(data_reader)), expected = 1)

  # Subsequent calls to get_data do not increment the 'called' counter...
  expect_true(is.data.frame(result$mtcars[[1]]$get_data()))
  expect_equal(get("called", pos = environment(data_reader)), expected = 1)

  # ...even when get_data is called on different configs (for the mtcars dataset)
  expect_true(is.data.frame(result$mtcars[[2]]$get_data()))
  expect_equal(get("called", pos = environment(data_reader)), expected = 1)
  expect_true(is.data.frame(result$mtcars[[6]]$get_data()))
  expect_equal(get("called", pos = environment(data_reader)), expected = 1)

  # The counter increments only when we move on to a different dataset.
  expect_true(is.data.frame(result$airquality[[4]]$get_data()))
  expect_equal(get("called", pos = environment(data_reader)), expected = 2)

  expect_true(is.data.frame(result$airquality[[1]]$get_data()))
  expect_equal(get("called", pos = environment(data_reader)), expected = 2)

  expect_true(is.data.frame(result$mtcars[[4]]$get_data()))
  expect_equal(get("called", pos = environment(data_reader)), expected = 2)

  expect_true(is.data.frame(result$attenu[[4]]$get_data()))
  expect_equal(get("called", pos = environment(data_reader)), expected = 3)

 })
