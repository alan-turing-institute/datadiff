#' Column accuracy evaluation metric
#'
#' Returns the average column accuracy observed in a synthetic experiment.
#'
#' This function will fail (with an error) unless the corruption in the synthetic
#' \code{experiment} is column-wise unique. In other words, it must
#' contain at most one patch of any given type applied to any given column
#' (see \code{\link{is_columnwise_unique}}). Although this is not strictly
#' required for calculating the column accuracy metric, it simplifies the
#' implementation and entails no loss of generality (since for any patch which
#' is not column-wise unique there is an equivalent one which is).
#'
#' @param experiment
#' An executed synthetic experiment object.
#' @param partial
#' A logical flag. If \code{TRUE}, partial credit is given (see the details for
#' the function \code{\link{pairwise_column_accuracy}}).
#' Defaults to \code{FALSE}.
#' @param column_param
#' A character vector containing the names of the column parameters in patches
#' of the relevant types. Defaults to \code{c("cols", "insertion_point")}.
#'
#' @return The observed column accuracy for each relevant corruption type as a
#' vector of numbers in the unit interval, with elements named by the patch type,
#' or \code{NA} if there are no relevant types in the corruption.
#'
#' @seealso \code{\link{pairwise_column_accuracy}}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' corruption <- sample_patch_rescale
#' config <- configure_synthetic_experiment(mtcars, corruption = corruption, N = 2)
#' output <- execute_synthetic_experiment(config)
#' metric_column_accuracy(output)
#' }
metric_column_accuracy <- function(experiment, partial = FALSE,
                                   column_param = c("cols", "insertion_point")) {

  # Vectorise over the column_param argument (retaining at most one NA).
  if (length(column_param) > 1) {
    lret <- purrr::map(column_param, .f = function(x) {
      metric_column_accuracy(experiment, partial = partial, column_param = x)
    })
    # If none of the column_params yield matching types, all results have no names.
    if (all(purrr::map_lgl(lret, .f = function(x) { is.null(names(x)) })))
      return(NA)
    return(unlist(purrr::discard(lret, .p = function(x) { is.null(names(x)) })))
  }

  stopifnot(executed(experiment))

  corruption <- experiment$get_corruption()
  types <- matching_types(corruption, param_name = column_param, short = TRUE)

  if (length(types) == 0)
    return(NA)

  # Calculate the column accuracy for each relevant patch type in the corruption.
  ret <- purrr::map_dbl(types, .f = function(type) {

    # Determine the column accuracy for each result w.r.t. the corruption.
    pairwise_result <- purrr::map_dbl(experiment$get_results(), .f = function(result) {
      pairwise_column_accuracy(target = corruption, result = result, type = type,
                               partial = partial, short = TRUE, column_param = column_param)
    })

    # Compute the average column accuracy.
    # old: sum(pairwise_result)/length(experiment$get_results())
    sum(pairwise_result, na.rm = TRUE)/sum(!is.na(pairwise_result))
  })

  names(ret) <- types
  ret
}


