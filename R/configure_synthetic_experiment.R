#' Configure a synthetic experiment
#'
#' Generates a \code{synthetic_experiment} object, configured according
#' to the given parameters.
#'
#' @details
#' The \code{seed} argument provides reproducibility in the context of random
#' sampling from patch distributions to obtain corruptions and random splitting
#' of the data.
#'
#' The \code{data_id} argument may be a data frame, the name of a data frame, or
#' a character vector specifying a data frame to be read via the given
#' \code{read_data} function, which itself defaults to \code{get()}.
#' If the \code{data_id} is a
#' character identifier the data are read lazily and may be stripped to save
#' space by calling \code{strip_data} on the returned \code{synthetic_experiment}
#' object.
#'
#' To execute a synthetic experiment, pass the return value to the function
#' \code{\link{execute_synthetic_experiment}}.
#'
#' @param data_id
#' A data frame or a character vector identifying a data frame. In the latter
#' case, the \code{read_data} argument must contain a function which returns a
#' data frame when passed the vector of arguments in \code{data_id}.
#' @param corruption
#' (Optional) A patch sampler function (or a list of such) to be used to generate
#' a patch with which to corrupt the second part of the random partition of the
#' \code{data}. If a list of sampler functions is provided then the generated
#' patches will be composed to obtain the corruption. Defaults to the identity
#' patch sampler (i.e. no corruption).
#' @param datadiff
#' The datadiff function for the experiment.
#' @param N
#' The number of experiments (i.e. random splits of the data). Defaults to 20.
#' @param split
#' A number in the unit interval specifying the splitting ratio.
#' @param seed
#' A random seed. By default an integer seed is chosen at random.
#' @param data_reader
#' A function which reads the data, given the arguments in \code{data_id}.
#' Defaults to the \code{\link{get}} function.
#'
#' @return A \code{synthetic_experiment} object.
#'
#' @seealso \code{\link{split_data}} \code{\link{execute_synthetic_experiment}}
#'
#' @examples
#' # Option 1: data frame passed as data_id argument. May bloat the config.
#' config <- configure_synthetic_experiment(data_id = mtcars)
#' head(config$get_data())
#'
#' # Option 2: custom data_reader argument (to avoid config bloat)
#' data_id <- "mtcars"
#' config <- configure_synthetic_experiment(data_id, data_reader = get)
#' head(config$get_data())
#'
#' \dontrun{
#' # Option 3: default data_reader argument is the \code{read_data} function.
#' data_id <- c("source" = "uci", "dataset" = "iris")
#' config <- configure_synthetic_experiment(data_id)
#' head(config$get_data())
#'
#' data_id <- c("source" = "data.gov.uk", "dataset" = "broadband", year = 2013)
#' config <- configure_synthetic_experiment(data_id)
#' head(config$get_data())
#' }
#' @export
configure_synthetic_experiment <- function(data_id,
                                           corruption = sample_patch_identity,
                                           datadiff = ddiff,
                                           N = 20,
                                           split = 0.5,
                                           seed = sample.int(.Machine$integer.max, size = 1),
                                           data_reader = get) {

  stopifnot(length(split) == 1)

  # Capture all config parameters in the current environment. Using an
  # environment enables us to assign the data to the Configuration object
  # inside the get_data function (and remove it inside the strip_data function)
  # to minimise data read
  ret <- environment()

  # Construct the 'get_corruption' function.
  get_corruption <- function() {

    if (!is.list(corruption))
      corruption <- list(corruption)

    # Set the seed before splitting the data. This ensures that the same rows
    # are always used when generating the corruption patch(es).
    set.seed(seed)
    df <- split_data(get_data(), split = split)[[2]]

    # Set the random seed immediately before generating the corruption patches.
    # This ensures that each call to get_corruption produces the same patch, but
    # corruptions of the same type will have different parameters.
    set.seed(seed)
    do.call(sample_patch, args = c(list(df = df), corruption))
  }

  # Construct the get_data & strip_data functions to provide access to the data
  # such that it is only read when necessary.
  strip_data <- function() {
    if (exists("data", envir = ret, inherits = FALSE))
      rm("data", pos = ret)
  }

  if (is.data.frame(data_id)) {
    args_list <- list()
    df <- data_id
    data_reader <- function(...) { df }
    data_id <- substitute(data_id)
  } else {
    args_list <- as.list(data_id)
  }
  get_data <- function() {
    if (exists("data", envir = ret, inherits = FALSE))
      return(get("data", envir = ret, inherits = FALSE))
    df <- do.call(data_reader, args = args_list)
    assign("data", value = df, pos = ret)
    df
  }

  class(ret) <- c("synthetic_experiment")
  ret
}
