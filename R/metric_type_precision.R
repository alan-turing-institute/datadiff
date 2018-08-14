#' Patch type precision metric
#'
#' Returns the patch type precision observed in a synthetic experiment.
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
#' metric_type_precision(output)
#' }
metric_type_precision <- function(experiment, short = TRUE, count_attr = "count") {

  stopifnot(executed(experiment))

  corruption <- experiment$get_corruption()

  if (is_identity_patch(corruption))
    return(NA)

  result_types_list <- lapply(experiment$get_results(), FUN = function(result) {
    patch_type(result, short = short, unique = TRUE)
  })

  types <- setdiff(unique(unlist(result_types_list)),
                   patch_type(patch_identity(), short = short))

  ret <- purrr::map(types, .f = function(type) {
    type_found_in_results <- purrr::map_lgl(result_types_list, .f = function(x) {
      type %in% x
    })
    type_found_in_corruption <- type %in%
      patch_type(corruption, short = short, unique = TRUE)

    # For each type, the metric is either zero or one. It is zero if the given
    # type is not found in the corruption, and one if it is. The count attribute
    # contains the information about the number of successful identifications.
    metric <- ifelse(type_found_in_corruption, yes = 1, no = 0)
    attr(metric, which = count_attr) <- sum(type_found_in_results)
    metric
  })
  names(ret) <- types

  ret
}

