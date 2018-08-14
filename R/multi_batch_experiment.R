#' Configure & execute a batch of synthetic experiments
#'
#' Use this function to run synthetic experiments sequentially on multiple
#' datasets and with multiple corruptions. To process the results, pass the
#' return value to the \code{batch_summary} function.
#'
#' @param data_ids
#' A collection of data frame identifiers. Each one must be a valid
#' \code{data_id} input to \code{\link{configure_synthetic_experiment}}.
#' @param corruptions
#' A collection of patch sampler functions. Each one must be a valid
#' \code{corruption} input to \code{\link{configure_synthetic_experiment}}.
#' @param datadiff
#' The datadiff function for the experiment.
#' @param N
#' The number of experiments (i.e. random splits of the data) per random seed.
#' @param M
#' The number of random seeds per corruption type.
#' @param split
#' A number in the unit interval specifying the splitting ratio.
#' @param hyperseed
#' A random seed used to select the \code{M} seeds for each corruption. By
#' default an integer seed is chosen at random.
#' @param data_reader
#' A function which reads the data, given each of the arguments in \code{data_id}.
#' @param execute
#' A logical flag. If \code{TRUE} (the default) the batch experiment will be
#' executed, and the outputs returned. Otherwise the batch of experiment
#' configurations will be returned without performing the experiments.
#' @param pb
#' A logical flag. If \code{TRUE} (the default) a progress bar will be
#' displayed in the console.
#' @param logfile
#' (Optional) The full path to a logfile.
#'
#' @return A list of lists of exectuted \code{synthetic_experiment} objects. The
#' outer list corresponds to the set of datasets specified in the \code{data_ids}
#' argument. The inner list corresponds to the list of \code{corruptions}.
#'
#' @export
#'
#' @seealso \code{\link{batch_experiment}}
#'
#'
#' @examples
#' \dontrun{
#' data_ids <- c("mtcars", "airquality")
#' corruptions <- list(sample_patch_identity,
#'                    list(sample_patch_delete, sample_patch_permute))
#' multi_batch_experiment(data_ids, corruptions = corruptions, N = 2, M = 3,
#'                        data_reader = get)
#' }
multi_batch_experiment <- function(data_ids,
                                   corruptions,
                                   datadiff = ddiff,
                                   N = 10,
                                   M = 10,
                                   split = 0.5,
                                   hyperseed = sample.int(.Machine$integer.max, size = 1),
                                   data_reader = get,
                                   execute = TRUE,
                                   pb = TRUE,
                                   logfile = NULL) {

  if (!is.null(logfile)) {
    logging::addHandler(logging::writeToFile, file=logfile)
    logging::loginfo("Multi batch experiment with %d dataset%s, N = %d, M = %d, hyperseed = %d",
                     length(data_ids), ifelse(length(data_ids) == 1, yes = "", no = "s"), N, M, hyperseed)
  }

  set.seed(hyperseed)

  # Loop over the different seeds per corruption
  seeds <- sample(.Machine$integer.max, size = M, replace = FALSE)
  results_by_seed <- as.list(rep(NA, times = length(seeds)))
  names(results_by_seed) <- as.character(seeds)
  for (j in seq_along(seeds)) {

    seed <- seeds[j]
    # The resut is a list of lists of synthetic experiment objects with the
    # outer list named by dataset and the inner list corresponding to the
    # corruptions.
    batch_output <- batch_experiment(data_ids = data_ids,
                                     corruptions = corruptions,
                                     datadiff = datadiff,
                                     N = N,
                                     split = split,
                                     seed = seed,
                                     execute = execute,
                                     data_reader = get,
                                     pb = TRUE)

    results_by_seed[[as.character(seed)]] <- batch_output
  }

  # Rearrange the list of batch results so that we can pass them directly to
  # the batch_summary and batch_failures_summary functions.
  ret <- purrr::map(data_ids, .f = function(data_id) {
    # Map over the seeds.
    unlist(purrr::map(names(results_by_seed), .f = function(s) {
      results_by_seed[[s]][[data_id]]
    }))
  })
  names(ret) <- data_ids

  if (!is.null(logfile))
    logging::loginfo("**** End of multi batch experiment ****")
  ret
}

