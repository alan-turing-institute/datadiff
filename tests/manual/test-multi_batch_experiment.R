require(testthat)
context("multi_batch_experiment function")

test_that("the multi_batch_experiment function works", {

  data_ids <- c("abalone", "iris", "heartdisease")
  corruptions <- list(sample_patch_identity,
                      sample_patch_rescale,
                      sample_patch_delete)
  datadiff <- purrr::partial(ddiff,
                      patch_generators = list(gen_patch_rescale, gen_patch_recode))
  N <- 3
  M <- 2
  split <- 0.5
  hyperseed <- 2222222

  result <- multi_batch_experiment(data_ids = data_ids,
                                   corruptions = corruptions,
                                   datadiff = datadiff,
                                   N = N,
                                   M = M,
                                   split = split,
                                   hyperseed = hyperseed,
                                   data_reader = data_reader)

  expect_equal(names(result), expected = data_ids)
  expect_true(all(purrr::map_lgl(data_ids, .f = function(data_id) {
    length(result[[data_id]]) == N*M
  })))

})
