#' Print a synthetic experiment object
#'
#' \code{print} prints its argument and returns it invisibly (via
#' \code{invisible(x)}).
#'
#' @param x
#' A \code{synthetic_experiment} object
#' @param ...
#' Any additional arguments are ignored.
#'
#' @keywords internal
#' @export
#'
#' @examples
#' config <- configure_synthetic_experiment("mtcars", data_reader = get)
#' config
#' # The printed functions names may be called on the config object:
#' config$get_corruption()
#'
print.synthetic_experiment <- function(x, ...) {

  state <- ifelse(executed(x), yes = "Output", no = "Configuration")
  cat("Synthetic Experiment", state, "\n")
  cat("  Data identifier:", paste(x$data_id, collapse = ", "), "\n")
  cat("  N:", x$N, "\n")
  cat("  split:", x$split, "\n")
  cat("  seed:", x$seed, "\n")
  function_names <- purrr::keep(names(x), .p = function(name) {
    is.function(x[[name]])
  })
  cat("  Functions:", paste(function_names, collapse = ", "), "\n")
  if (executed(x))
    cat("  Execution_time:", format(x$execution_time, digits = 2), "\n")
  invisible(x)
}

#' Determine whether a synthetic experiment has been executed
#'
#' @param x
#' A \code{synthetic_experiment} object
#'
#' @export
#'
#' @examples
#' config <- configure_synthetic_experiment("mtcars", N = 2, data_reader = get)
#' executed(config)
#'
#' \dontrun{
#' execute_synthetic_experiment(config)
#' executed(config)
#' }
#'
executed <- function(x) {

  stopifnot(is_synthetic_experiment(x))
  all(c("results", "execution_time") %in% names(x))
}

#' Test for a synthetic experiment object
#'
#' Returns \code{TRUE} iff the \code{obj} argument is a \code{synthetic
#' experiment} object.
#'
#' @param obj
#' Any object.
#'
#' @export
#'
#' @examples
#' config <- configure_synthetic_experiment("mtcars", data_reader = get)
#' is_synthetic_experiment(config)
#' is_synthetic_experiment(mtcars)
#'
is_synthetic_experiment <- function(obj) {
  methods::is(obj, "synthetic_experiment")
}
