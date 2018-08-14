#' Parameter RMSE metric
#'
#' Returns the parameter root mean square error (RMSE) observed in a synthetic
#' experiment. Note that the parameter square error is only reported for
#' experiment results which are column-accurate and therefore it is important to
#' take the into account the column accuracy metric when interpreting the
#' parameter RMSE results. The count attribute (see the \code{count_attr} argument)
#' serves a similar purpose.
#'
#' Returns \code{NA} if there are no patch types in the elementary decomposition
#' of the corruption that contain real-valued parameters.
#'
#' @param experiment
#' An executed synthetic experiment object.
#' @param column_param
#' The name of the column parameter in patches of the relevant types.
#' @param count_attr
#' The name of the "count" attribute used to store the number of terms in the
#' sum of square errors (i.e. not including NAs) in the computation of the RMSE.
#' This attribute is intended as an aid when interpreting the results. Set this
#' argument to \code{NULL} to withdraw this attribute. The default value is
#' \code{count}.
#'
#' @return The observed parameter root mean square error for each relevant
#' patch type as a list of numeric vectors. The list elements are named according
#' to the patch types. The numeric vector element names correspond to the
#' real-valued parameter names.
#'
#' @seealso \code{\link{metric_parameter_comparison}}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' corruption <- sample_patch_rescale
#' config <- configure_synthetic_experiment(mtcars, corruption = corruption, N = 2)
#' output <- execute_synthetic_experiment(config)
#' metric_parameter_RMSE(output)
#' }
metric_parameter_RMSE <- function(experiment, column_param = "cols",
                                  count_attr = "count") {

  f_metric <- purrr::compose(sqrt, mean)
  f_comparison <- function(x, y) { (x - y)^2 }
  f_map <- purrr::map_dbl
  f_param_match <- is.double

  metric_parameter_comparison(experiment = experiment,
                              f_metric = f_metric,
                              f_comparison = f_comparison,
                              f_map = f_map,
                              f_param_match = f_param_match,
                              column_param = column_param,
                              count_attr = count_attr)
}
