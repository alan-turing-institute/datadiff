#' Hamming distance metric for permutations
#'
#' Returns the mean Hamming distance for permutations observed in a synthetic
#' experiment. Both the corruption and the results are assumed to contain at
#' most one permutation patch (otherwise an error is thrown).
#'
#' Returns \code{NA} if there are no patch types in the elementary decomposition
#' of the corruption that contain the given permutation parameter.
#'
#' @param experiment
#' An executed synthetic experiment object.
#' @param perm_param
#' The name of the permutation parameter in patches of the relevant types.
#' @param count_attr
#' The name of the "count" attribute used to store the number of terms averaged
#' in the the computation of the mean Hamming distance. Set this argument to
#' \code{NULL} to omit this attribute. The default value is \code{count}.
#'
#' @return A numeric scalar containing the mean observed Hamming distance
#' between the permutation in the corruption and that in the results.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' corruption <- purrr::partial(sample_patch_permute, n = 2L)
#' config <- configure_synthetic_experiment(mtcars, corruption = corruption, N = 2)
#' output <- execute_synthetic_experiment(config)
#' metric_hamming_distance(output)
#' }
metric_hamming_distance <- function(experiment, perm_param = "perm",
                                    count_attr = "count") {

  stopifnot(executed(experiment))
  corruption <- experiment$get_corruption()

  if (!any(purrr::map_lgl(decompose_patch(corruption), .f = function(p) {
    perm_param %in% names(get_patch_params(p))
  })))
    return(NA)

  # Identify the elementary patch type having the given permutation parameter.
  type <- matching_types(corruption, param_name = perm_param, short = TRUE)

  pairwise_hamming_distance <- function(target, result) {

    # TODO:
    # If necessary, take into account any insertions or deletions that occur
    # *before or after* the permutation (in the target and/or the result).

    canonical_target <- extract_canonical_permutation(target,
                                                      perm_param = perm_param,
                                                      param_only = TRUE)
    canonical_result <- extract_canonical_permutation(result,
                                                      perm_param = perm_param,
                                                      param_only = TRUE)

    # If the result contains no permutation component, but the target does,
    # compare the target to the identity permutation.
    if (is.na(canonical_result) && !is.na(canonical_target))
      canonical_result <- 1:length(canonical_target)

    stopifnot(setequal(canonical_target, canonical_result))
    sum(canonical_target != canonical_result)
  }

  # Calculate the pairwise Hamming distances for each corruption-result pair.
  all_phd <- purrr::map_int(experiment$get_results(), .f = function(result) {
    pairwise_hamming_distance(target = corruption, result = result)
  })

  phd <- all_phd[!is.na(all_phd)]
  ret <- mean(phd)

  # Attach an attribute containing the number of terms in the calcuation of the mean.
  if (!is.null(count_attr))
    attr(ret, which = count_attr) <- length(phd)

  names(ret) <- type
  ret
}
