#' Summarise the failures in a batch of synthetic experiments
#'
#' Prints to the console a summary of failed experiments in a batch.
#'
#' @param batch_output
#' A list of lists of executed \code{synthetic_experiment} objects, as returned
#' by the \code{batch_experiment} function.
#'
#' @seealso \code{\link{batch_experiment}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data_ids <- c("cars")
#' corruptions <- list(sample_patch_identity,
#'                    sample_patch_permute,
#'                    sample_patch_delete,
#'                    list(sample_patch_insert, sample_patch_permute))
#' batch_output <- batch_experiment(data_ids, corruptions = corruptions, N = 2,
#'                    data_reader = get)
#' # Experiment 3 failed because the 'cars' dataset has only two columns.
#' batch_failures_summary(batch_output)
#' }
batch_failures_summary <- function(batch_output) {

  sink <- sapply(names(batch_output), FUN = function(data_id) {

    success <- sapply(batch_output[[data_id]], FUN = function(x) { executed(x) })

    if (all(success))
      return()

    sink <- sapply(which(!success), FUN = function(i) {
      message(paste(data_id, i, sep = ":"))
      output <- batch_output[[data_id]][[i]]
      tryCatch( {
        print(output$get_corruption())
      },
      error = function(e) {
        message("Failed to sample corruption patch.")
      })
      message(output$err_msg())
      message(paste(sapply(output$get_data(), typeof), collapse = " "))
      message("\n")
    })
  })
}
