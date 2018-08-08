#' Parameter accuracy metric
#'
#' Returns the mean parameter accuracy observed in a synthetic experiment.
#' Pairwise parameter accuracy is a Boolean quantity: it is \code{TRUE} iff a
#' pair of vector (or list) parameters define the same mapping between element
#' names and values. The parameter accuracy metric is the average pairwise
#' accuracy between the corruption and the experiment results (after converting
#' each Boolean pairwise result to a binary digit).
#'
#' Returns \code{NA} if there are no patch types in the elementary decomposition
#' of the corruption that contain the given parameter name.
#'
#' @param experiment
#' An executed synthetic experiment object.
#' @param column_param
#' The name of the column parameter in patches of the relevant types.
#' @param count_attr
#' The name of the "count" attribute used to store the number of terms averaged
#' in the the computation of the mean parameter accuracy.
#' This attribute is intended as an aid when interpreting the results. Set this
#' argument to \code{NULL} to withdraw this attribute. The default value is
#' \code{count}.
#'
#' @return A numeric scalar containing the mean observed parameter accuracy.
#'
#' @seealso \code{\link{metric_parameter_comparison}}
#'
#' @export
metric_parameter_accuracy <- function(experiment, column_param = "cols",
                                      count_attr = "count") {

  f_metric <- purrr::compose(mean, as.numeric)

  # Arguments to pairwise_parameter_comparison to perform a Boolean test for
  # pairwise accuracy of parameters which are vectors (or lists) with named
  # elements, where the result is TRUE if and only if the mapping between element
  # names and values is identical.
  f_comparison <- function(x, y) {
    if (is.na(x) || is.na(y))
      return(NA)
    if (!setequal(names(x), names(y)))
      return(FALSE)
    if (!setequal(x, y))
      return(FALSE)
    all(purrr::map_lgl(names(x), .f = function(name) {
      x[name] == y[name]
    }))
  }

  f_map <- purrr::map_lgl

  f_param_match <- function(x) {
    is.vector(x)
    length(x) > 1 && length(names(x)) == length(x)
  }

  metric_parameter_comparison(experiment = experiment,
                              f_metric = f_metric,
                              f_comparison = f_comparison,
                              f_map = f_map,
                              f_param_match = f_param_match,
                              column_param = column_param,
                              count_attr = count_attr)
}
