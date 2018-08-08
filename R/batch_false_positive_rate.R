#' Compute the false positive rate for a batch of synthetic experiments
#'
#' @param batch_output
#' A list of lists of executed \code{synthetic_experiment} objects, as returned
#' by the \code{batch_experiment} function.
#' @param aggregated
#' A logical flag. Defaults to \code{TRUE}.
#' @param count_attr
#' The name of the "count" attribute used to store the number of terms in the
#' computation of the average false positive rate. The default value is
#' \code{count}.
#'
#' @seealso \code{\link{batch_experiment}}
#'
#' @return The average false positive rate for the batch (with a count attribute
#' containing the number of individual resamplings of the data involved) or, if
#' aggregated is \code{FALSE}, a list with an average false positive rate for
#' each dataset (each with a count attribute).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' batch_output <- batch_experiment("mtcars",
#'                                  corruptions = sample_patch_identity,
#'                                  N = 2,
#'                                  data_reader = get)
#' batch_false_positive_rate(batch_output)
#' }
batch_false_positive_rate <- function(batch_output, aggregated = TRUE,
                                      count_attr = "count") {

  data_ids <- names(batch_output)
  stopifnot(length(data_ids) == length(batch_output))

  fpr_by_data_id <- purrr::map(data_ids, .f = function(data_id) {

    # Keep only those experiments which are executed and have the identity
    # patch as their corruption.
    outputs <- purrr::keep(batch_output[[data_id]], .p = function(output) {
      stopifnot(is_synthetic_experiment(output))
      executed(output) && (is_identity_patch(output$get_corruption()))
    })
    # Get the false positive rates for each experiment.
    fprs <- purrr::map_dbl(outputs, .f = false_positive_rate)
    stopifnot(all(!is.na(fprs)))

    # Get the number of resamplings in each experiment.
    Ns <- purrr::map_int(outputs, .f = function(output) { as.integer(output$N) })

    # Return the average false positive rate, weighted by N
    ret <- as.numeric(t(Ns) %*% fprs)/sum(Ns)
    attr(ret, which = count_attr) <- sum(Ns)
    ret
  })
  names(fpr_by_data_id) <- data_ids

  if (!aggregated)
    return(fpr_by_data_id)

  # Compute the overall average.
  weights <- purrr::map_dbl(fpr_by_data_id, .f = attr, which = count_attr, exact = TRUE)
  ret <- as.numeric(weights %*% unlist(fpr_by_data_id))/sum(weights)
  attr(ret, which = count_attr) <- sum(weights)
  ret
}
