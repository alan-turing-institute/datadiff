#' False positive rate
#'
#' Returns the false positive rate observed in a synthetic experiment.
#'
#' @param experiment
#' An executed synthetic experiment object.
#' @param type
#' One or more patch types. If \code{NULL} (the default), false positives of
#' all patch types are included in a single result.
#' @param short
#' A logical flag passed to the \code{\link{patch_type}} function. If
#' \code{TRUE} (the default) the \code{type} argument must specify the patch
#' type in short form.
#'
#' @return The false positive rate as a number in the unit interval. If
#' \code{type} is not \code{NULL}, a vector of false positive rates is returned
#' with patch types for element names.
#'
#' @seealso \code{\link{execute_synthetic_experiment}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' config <- configure_synthetic_experiment("mtcars", N = 2, data_reader = get)
#' output <- execute_synthetic_experiment(config)
#' false_positive_rate(output)
#' }
false_positive_rate <- function(experiment, type = NULL, short = TRUE) {

  corruption <- experiment$get_corruption()

  if (!is_identity_patch(corruption))
    return(NA)

  is_not_false_positive <- purrr::map_lgl(experiment$results, is_identity_patch)

  if (is.null(type))
    return(sum(!is_not_false_positive)/length(is_not_false_positive))

  ret <- purrr::map_dbl(type, .f = function(t) {
    if (all(is_not_false_positive))
      return(0)
    is_matching_type <- purrr::map_lgl(experiment$results[!is_not_false_positive],
                                       .f = function(p) { t %in% patch_type(p) })
    sum(is_matching_type)/length(is_not_false_positive)
  })
  names(ret) <- type
  ret
}

