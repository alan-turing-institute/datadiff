#' Parameter comparison metric computation
#'
#' Returns the parameter comparison metric observed in a synthetic experiment.
#' Note that the calculation of parameter comparison metrics ignores experiment
#' results which are not column-accurate and therefore it is important to take
#' into account the column accuracy metric when interpreting parameter
#' comparison metrics. The count attribute (see the \code{count_attr} argument)
#' serves a similar purpose.
#'
#' Returns \code{NA} if there are no patch types in the elementary decomposition
#' of the corruption that contain matching parameters (as identified by the
#' \code{f_param_match} argument.
#'
#' @param experiment
#' An executed synthetic experiment object.
#' @param f_metric
#' A function for computing the comparison metric from a vector of pairwise
#' comparisons.
#' @param f_comparison
#' A function for pairwise parameter comparison.
#' @param f_map
#' A map function (e.g. \code{purrr::map_dbl} is appropriate in case
#' \code{f_comparison} returns a scalar double value).
#' @param f_param_match
#' A predicate for matching parameters based on their value. For example, if
#' the parameter of interest is a double value, the \code{is.double} function
#' is appropriate here.
#' @param column_param
#' The name of the column parameter in patches of the relevant types.
#' @param count_attr
#' The name of the "count" attribute used to store the number of terms in the
#' computation of the metric (not including NAs). This attribute is intended as
#' an aid when interpreting the results. Set this argument to \code{NULL} to
#' withdraw this attribute. The default value is \code{count}.
#'
#' @return The observed parameter comparison metric for each relevant
#' patch type as a list of numeric vectors. The list elements are named according
#' to the patch types. The numeric vector element names correspond to the
#' matching parameter names (i.e. those to which the particular metric applies,
#' as determined by the \code{f_param_match} function).
#'
#' @seealso \code{\link{pairwise_parameter_comparison}}
#' \code{\link{metric_parameter_RMSE}} \code{\link{metric_parameter_accuracy}}
#'
#' @export
metric_parameter_comparison <- function(experiment, f_metric, f_comparison, f_map,
                                        f_param_match, column_param = "cols",
                                        count_attr = "count") {

  stopifnot(executed(experiment))
  corruption <- experiment$get_corruption()

  # Identify all elementary patch types having the given column index parameter.
  all_types <- matching_types(corruption, param_name = column_param,
                              short = TRUE)

  # Keep only those elementary patch types which contain a matching parameter.
  types <- purrr::keep(all_types, .p = function(type) {
    ps <- purrr::keep(decompose_patch(corruption), .p = function(p) {
      patch_type(p, short = TRUE) == type
    })
    any(purrr::map_lgl(ps, .f = function(p) {
      any(purrr::map_lgl(get_patch_params(p), .f = f_param_match))
    }))
  })

  # Return NA if there are no matching types found in the corruption.
  if (length(types) == 0)
    return(NA)

  # Calculate the metric for each relevant patch type in the corruption.
  ret <- purrr::map(types, .f = function(type) {

    # Calculate the pairwise metric comparisons for each corruption-result pair.
    pmcs <- purrr::map(experiment$get_results(), .f = function(result) {
      pairwise_parameter_comparison(corruption, result = result, type = type,
                                    f_comparison = f_comparison, f_map = f_map,
                                    f_param_match = f_param_match, short = TRUE,
                                    column_param = column_param)
    })

    # If patches of this type do not contain matching parameters, pmcs will
    # contain only NA values. In this case, return NA.
    if (all(is.na(pmcs)))
      return(NA)

    # Map over the relevant parameter names & calculate the metric.
    params <- unique(unlist(purrr::map(pmcs, .f = names)))
    comparisons <- NULL
    metric <- purrr::map_dbl(params, .f = function(param) {

      # Get all of the comparisons for each parameter name. If there are
      # multiple columns corrupted by the same patch type, this vector will
      # contain the comparisons for all of them (as required).
      all_comparisons <- unlist(purrr::map(pmcs, .f = function(x) {
        if (!(param %in% names(x)))
          return(NA)
        x[[param]]
      }))

      if (all(is.na(all_comparisons)))
        return(NA)

      # Discard any NAs
      comparisons <<- all_comparisons[!is.na(all_comparisons)]

      # Compute the metric.
      f_metric(comparisons)
    })

    # Attach the "count" attribute. This contains the number of terms in the
    # computation of the metric (e.g. terms in the sum of square errors, when
    # the metric is the RMSE), as an aid when interpreting the results.
    if (!is.null(count_attr))
      attr(metric, which = count_attr) <- length(comparisons)
    names(metric) <- params
    metric
  })

  names(ret) <- types
  ret
}
