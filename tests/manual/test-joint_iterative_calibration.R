require(testthat)
context("joint_iterative_calibration")

test_that("the joint_iterative_calibration function works", {

  ####
  #### Test with the UCI datasets.
  ####
  data_ids <- c("abalone", "iris", "car")
  # data_ids <- setdiff(datasets("uci"), "00240")
  data_reader <- read_data
  datadiff <- ddiff
  patch_generators <- list(gen_patch_rescale, gen_patch_recode)
  patch_penalties <- c(0.33, 0.33)
  permute_penalty <- 0.33
  break_penalty <- 0.95
  N <- 3
  M <- 4
  split <- 0.5
  hyperseed <- .Machine$integer.max
  target_fpr <- 0.2
  # Test with a wide acceptance margin.
  acceptance_margin <- 0.5
  increment_factor <- 1.2
  decrement_factor <- 0.8
  lower_boundary <- 10^(-6)
  check_viability <- FALSE
  pb = TRUE

  result <- joint_iterative_calibration(data_ids = data_ids,
                                  datadiff = datadiff,
                                  patch_generators = patch_generators,
                                  patch_penalties = patch_penalties,
                                  permute_penalty = permute_penalty,
                                  break_penalty = break_penalty,
                                  N = N,
                                  M = M,
                                  split = split,
                                  hyperseed = hyperseed,
                                  data_reader = data_reader,
                                  target_fpr = target_fpr,
                                  acceptance_margin = acceptance_margin,
                                  increment_factor = increment_factor,
                                  decrement_factor = decrement_factor,
                                  lower_boundary = lower_boundary,
                                  check_viability = check_viability,
                                  pb = pb)

  # The return value is a numeric vector with element names corresponding
  # to patch types for which the penalty parameter was calibrated.
  expect_true(is.numeric(result))
  expect_equal(names(result), c("rescale", "recode", "permute"))


  # ####
  # #### Test with the broadband 2013 dataset.
  # ####
  # data_ids <- "broadband"
  # data_reader <- purrr::partial(read_data, source = "data.gov.uk", year = 2013)
  # datadiff <- ddiff
  # patch_generators <- list(gen_patch_rescale, gen_patch_recode)
  # # permute_penalty <- 2
  # # patch_penalties <- c(12, 16)
  # patch_penalties <- c(10, 22)
  # permute_penalty <- 10
  #
  # # break_penalty <- 0.95
  # # patch_penalties <- c(1, 3)
  # permute_penalty <- 0.5
  # break_penalty <- 0.95
  # N <- 3
  # M <- 4
  # split <- 0.5
  # hyperseed <- .Machine$integer.max
  # #target_fpr <- 0.05
  # target_fpr <- 0.1
  # # Test with a wide acceptance margin.
  # acceptance_margin <- 0.5
  # increment_factor <- 1.2
  # decrement_factor <- 0.8
  # lower_boundary <- 10^(-6)
  # upper_boundary <- 20
  # check_viability <- FALSE
  # pb = TRUE
  #
  # result <- joint_iterative_calibration(data_ids = data_ids,
  #                                       datadiff = datadiff,
  #                                       patch_generators = patch_generators,
  #                                       patch_penalties = patch_penalties,
  #                                       permute_penalty = permute_penalty,
  #                                       break_penalty = break_penalty,
  #                                       N = N,
  #                                       M = M,
  #                                       split = split,
  #                                       hyperseed = hyperseed,
  #                                       data_reader = data_reader,
  #                                       target_fpr = target_fpr,
  #                                       acceptance_margin = acceptance_margin,
  #                                       increment_factor = increment_factor,
  #                                       decrement_factor = decrement_factor,
  #                                       lower_boundary = lower_boundary,
  #                                       upper_boundary = upper_boundary,
  #                                       check_viability = check_viability,
  #                                       pb = pb)
  #
  # # The return value is a numeric vector with element names corresponding
  # # to patch types for which the penalty parameter was calibrated.
  # expect_true(is.numeric(result))
  # expect_equal(names(result), c("rescale", "recode", "permute"))

})
