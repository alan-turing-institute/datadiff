#' Patch type recall evaluation metric
#'
#' Returns the patch type recall rate observed in a synthetic experiment.
#'
#' @param experiment
#' An executed synthetic experiment object.
#' @param short
#' A logical flag passed to the \code{\link{patch_type}} function.
#' @param count_attr
#' A character string specifying the name of the 'count' attribute.
#'
#' @return A list of numbers, each in the unit interval, each with an element
#' name corresponding to the patch type and a count attribute which counts the
#' number of terms from which the metric was calculated.
#'
#' @seealso \code{\link{execute_synthetic_experiment}}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' corruption <- sample_patch_rescale
#' config <- configure_synthetic_experiment(mtcars, corruption = corruption, N = 2)
#' output <- execute_synthetic_experiment(config)
#' metric_type_recall(output)
#' }
metric_type_recall <- function(experiment, short = TRUE, count_attr = "count") {

  stopifnot(executed(experiment))

  corruption <- experiment$get_corruption()

  if (is_identity_patch(corruption))
    return(NA)

  corruption_types <- patch_type(corruption, short = short, unique = TRUE)

  ret <- purrr::map(corruption_types, .f = function(type) {
    type_found_in_results <- purrr::map_lgl(experiment$get_results(), .f = function(x) {
      type %in% patch_type(x, short = short, unique = TRUE)
    })

    # Compute the metric as the frequency with which the type is found in the result.
    metric <- sum(type_found_in_results)/length(type_found_in_results)
    attr(metric, which = count_attr) <- length(type_found_in_results)
    metric
  })
  names(ret) <- corruption_types

  ret
}

