require(testthat)
context("pairwise_parameter_comparison function")

test_that("the pairwise_parameter_comparison function works", {

  # square error parameters for testing (see metric_parameter_RMSE):
  f_comparison <- function(x, y) { (x - y)^2 }
  f_map <- purrr::map_dbl
  f_param_match <- is.double

  #### Test with the rescale patch type
  type <- "rescale"

  # Patch taken from batch experiment run on 22/01/2018.
  # wine-quality dataset, experiment 12, result 2.
  target <- patch_rescale(cols = 5L, shift = -0.00535989, scale_factor = 1.929871)

  p1 = patch_rescale(cols = 5L, shift = -31.58284, scale_factor = 3876.67)
  p2 = patch_rescale(cols = 7L, shift = 0.01010382, scale_factor = 0.0004900724)

  result_patch <- compose_patch(p2, p1)

  result <- pairwise_parameter_comparison(target, result = result_patch,
                                          type = type,
                                          f_comparison = f_comparison,
                                          f_map = f_map,
                                          f_param_match = f_param_match)

  # Here we expect a numeric result containing the square error
  expected <- list(c("5" = (-0.0053598 - (-31.58284))^2), c("5" = (1.929871 - 3876.67)^2))
  names(expected) <- c("shift", "scale_factor")
  expect_equal(result, expected = expected)

  # Now include a permutation patch which moves column 5.
  p3 = patch_permute(perm = as.integer(c(1, 2, 3, 4, 7, 6, 5, 8, 9, 10, 11, 12)))
  result_patch <- compose_patch(p3, p2, p1)

  result <- pairwise_parameter_comparison(target, result = result_patch,
                                          type = type,
                                          f_comparison = f_comparison,
                                          f_map = f_map,
                                          f_param_match = f_param_match)

  # We now expect an NA result because the relevant column (colun 5) is not in
  # the same position after application of the result_patch vs. the target patch.
  expected <- list(c("5" = as.numeric(NA)), c("5" = as.numeric(NA)))
  names(expected) <- c("shift", "scale_factor")
  expect_equal(result, expected = expected)
})
