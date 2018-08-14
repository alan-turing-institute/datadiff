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
#' The number of experiments (i.e. random splits of the data) in each batch.
#' Defaults to 20.
#' @param split
#' A number in the unit interval specifying the splitting ratio.
#' @param seed
#' A random seed. By default an integer seed is chosen at random.
#' @param data_reader
#' A function which reads the data, given each of the arguments in \code{data_id}.
#' @param execute
#' A logical flag. If \code{TRUE} (the default) the batch experiment will be
#' executed, and the outputs returned. Otherwise the batch of experiment
#' configurations will be returned without performing the experiments.
#' @param pb
#' A logical flag. If \code{TRUE} (the default) a progress bar will be
#' displayed in the console.
#' @param quiet
#' A logical flag. If \code{TRUE} an error message will be printed to the
#' console in the event of a failure to execute any particular synthetic
#' experiment in the batch. Defaults to \code{FALSE}.
#' @param logfile
#' (Optional) The full path to a logfile.
#'
#' @return A list of lists of exectuted \code{synthetic_experiment} objects. The
#' outer list corresponds to the set of datasets specified in the \code{data_ids}
#' argument. The inner list corresponds to the list of \code{corruptions}.
#'
#' @seealso \code{\link{configure_synthetic_experiment}}
#' \code{\link{execute_synthetic_experiment}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data_ids <- c("mtcars", "airquality")
#' corruptions <- list(sample_patch_identity,
#'                    sample_patch_permute,
#'                    sample_patch_insert,
#'                    list(sample_patch_delete, sample_patch_permute))
#' batch_experiment(data_ids, corruptions = corruptions, N = 2, data_reader = get)
#' }
batch_experiment <- function(data_ids,
                             corruptions,
                             datadiff = ddiff,
                             N = 20,
                             split = 0.5,
                             seed = sample.int(.Machine$integer.max, size = 1),
                             data_reader = get,
                             execute = TRUE,
                             pb = TRUE,
                             quiet = TRUE,
                             logfile = NULL) {

  if (!is.null(logfile)) {
    logging::addHandler(logging::writeToFile, file=logfile)
    logging::loginfo("Batch experiment with %d dataset%s, N = %d, seed = %d",
                     length(data_ids), ifelse(length(data_ids) == 1, yes = "", no = "s"), N, seed)
  }

  stopifnot(length(split) == 1)
  if (is.data.frame(data_ids))
    data_ids <- list(data_ids)
  if (!is.list(corruptions))
    corruptions <- list(corruptions)

  if (pb && execute)
    .pb <- progress::progress_bar$new(total = length(data_ids) * length(corruptions))

  # For each data_id, map over the corruptions to construct the config objects,
  # then update the get_data functions to minimise calls to read the data, then
  # map over the config objects to execute the experiments (if execute is TRUE).
  # Note that we must force the evaluation of the corruption formal argument
  # otherwise only the last corruption will be captured (due to lazy evaluation).
  ret <- purrr::map(data_ids, .f = function(data_id) {
    configs <- purrr::map(corruptions, .f = function(corruption) {
      force(corruption)
      configure_synthetic_experiment(data_id = data_id,
                                     corruption = corruption,
                                     datadiff = datadiff,
                                     N = N,
                                     split = split,
                                     seed = seed,
                                     data_reader = data_reader)
    })

    # If there are multiple corruptions (hence multiple configs for this dataset)
    # assign the get_data function from the first config to all of the configs
    # for this dataset, so that each dataset will need to be read only once for
    # the entire batch of experiments on that data.
    if (length(configs) > 1)
      sink <- sapply(configs, FUN = function(config) {
        config$get_data <- configs[[1]]$get_data
      })

    if (!execute)
      return(configs)

    if (is.character(data_id) && !is.null(logfile))
      logging::loginfo("Dataset: %s", data_id)

    purrr::map(configs, .f = function(config) {
      if (pb)
        .pb$tick()

      # Wrap the experiment execution in a try-catch. An error is expected only
      # in case the corruption is incompatible with the data frame (e.g. a
      # shift/scale cannot be applied to a data frame containing no real-valued
      # columns).
      tryCatch(
        execute_synthetic_experiment(config, logfile = logfile),
        error = function(err) {
          if (!quiet)
            message(err)

          # TODO: include the corruption type in the logged error message.
          # The type must be extracted from the patch sampler functions (via
          # config$corruption), rather than using config$get_corruption(), since
          # the error may be due to failure in generating the corruption.
          if (!is.null(logfile))
            logging::logerror("Synthetic experiment failed:\n%s", err)

          # Add the error to the config (environment).
          err_msg <- function() { err$message }
          assign("err", value = err, pos = config)
          assign("err_msg", value = err_msg, pos = config)
          config
        },
        finally = {
          # Ensure the data are stripped. If there are multiple configs, strip
          # the data from the first config environment (see above comment).
          config$strip_data()
          if (length(configs) > 1)
            configs[[1]]$strip_data()
        })
    })
  })
  if (is.character(data_ids))
    names(ret) <- purrr::map_chr(data_ids,
                                 purrr::partial(paste, collapse = "_"))
  if (!is.null(logfile))
    logging::loginfo("---- End of batch experiment ----")

  ret
}
