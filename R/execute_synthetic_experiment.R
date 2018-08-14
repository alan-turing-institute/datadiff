#' Execute a synthetic experiment
#'
#' Executes a synthetic experiment based on a given configuration object and
#' returns the same object with results attached, accessible via the
#' \code{get_results} method.
#'
#' @param config
#' A \code{synthetic_experiment} object specifying the experiment configuration.
#' @param logfile
#' (Optional) The full path to a logfile.
#'
#' @return An executed \code{synthetic_experiment} object.
#'
#' @seealso \code{\link{configure_synthetic_experiment}}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' corruption <- list(sample_patch_identity, sample_patch_permute)
#' config <- configure_synthetic_experiment(mtcars, corruption = corruption, N = 2)
#' execute_synthetic_experiment(config)
#' }
execute_synthetic_experiment <- function(config, logfile = NULL) {

  if (!is.null(logfile))
    logging::addHandler(logging::writeToFile, file=logfile, level='DEBUG')

  stopifnot(is_synthetic_experiment(config))

  # Get the data & corruption, then strip the data (to avoid bloating).
  data <- config$get_data()
  corruption <- config$get_corruption()

  config$strip_data()

  if (!is.null(logfile))
    logging::loginfo("Synthetic experiment with N = %d, corruption: %s",
                   config$N, paste(patch_type(corruption), collapse = ", "))

  # Use the random seed (from the config) for reproducibility.
  set.seed(config$seed)

  start_time <- Sys.time()
  results <- purrr::map(1:config$N, .f = function(i) {

    # Split the data and call datadiff on the two pieces.
    dfs <- split_data(data, split = config$split)
    config$datadiff(dfs[[1]], corruption(dfs[[2]]))
  })
  get_results <- function() { results }
  execution_time <- Sys.time() - start_time

  # Add the results to the config (environment).
  assign("results", value = results, pos = config)
  assign("get_results", value = get_results, pos = config)
  assign("execution_time", value = execution_time, pos = config)

  config
}
