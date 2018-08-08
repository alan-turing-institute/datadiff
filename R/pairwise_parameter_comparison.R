#' Compute a pairwise parameter comparison
#'
#' Returns a pairwise parameter comparison of a \code{result} w.r.t. the
#' \code{target} patch.
#'
#' The result is a list with a named element for each parameter associated with
#' the given patch \code{type}. Each list element is a vector with named elements
#' corresponding to the intitial positions of the columns that are transformed
#' by the \code{target} and \code{result} patches. Vector element values
#' contain the computed parameter comparison, or \code{NA} if either:
#'  - the initial column position appears in the target but not the result, or
#'  - the initial column position is moved by the result patch to a different
#'    position than that to which it is moved by the target patch.
#'
#' Returns NA if the (partial) pairwise column accuracy of the \code{result}
#' w.r.t. the \code{target} is zero (i.e. if none of the columns transformed by
#' the target \code{patch} are correctly identified in the \code{result}), or if
#' the final positions of patched columns differ after application of the result
#' and target patches.
#'
#' @param target
#' A patch.
#' @param result
#' A patch.
#' @param type
#' A patch type.
#' @param f_comparison
#' A function for pairwise parameter comparison.
#' @param f_map
#' A map function (e.g. \code{purrr::map_dbl} is appropriate in case
#' \code{f_comparison} returns a scalar double value).
#' @param f_param_match
#' A predicate for matching parameters based on their value. For example, if
#' the parameter of interest is a double value, the \code{is.double} function
#' is appropriate here.
#' @param short
#' A logical flag passed to the \code{\link{patch_type}} function. If
#' \code{TRUE} (the default) the \code{type} argument must specify the patch
#' type in short form.
#' @param column_param
#' The name of the column parameter in patches of the specified type.
#'
#' @return A named list of non-negative vectors correpsonding to the parameters
#' in the given patch type. Each vector has elements whose type is determined by
#' \code{f_map} (or NA if the (partial) pairwise column accuracy of the
#' \code{result} w.r.t. the \code{target} is zero) with named elements
#' corresponding to the initial position of the column that is transformed by
#' the \code{target} and \code{result} patches. The vectors are ordered by the
#' element names.
#'
#' @export
pairwise_parameter_comparison <- function(target, result, type, f_comparison,
                                          f_map = purrr::map_dbl, f_param_match,
                                          short = TRUE, column_param = "cols") {

  pca <- pairwise_column_accuracy(target = target, result = result, type = type,
                                  partial = TRUE, short = short,
                                  column_param = column_param)

  # If the result is not (at least partially) column accurate, return NA.
  if (is.na(pca) || pca == 0)
    return (NA)

  param_values <- function(patch) {

    # Identify all elementary components of the given type.
    decomposed <- decompose_patch(patch)
    type_match <- purrr::map_lgl(decomposed, .f = function(p) {
      identical(patch_type(p, short = short), type)
    })

    # Check that all elementary patches matching the given type contain the
    # column index parameter.
    stopifnot(all(purrr::map_lgl(decomposed[type_match], .f = function(p) {
      column_param %in% names(get_patch_params(p))
    })))

    # Identify all real-valued parameter names.
    params <- unique(unlist(purrr::map(decomposed[type_match], .f = function(p) {
      param_match <- purrr::map_lgl(get_patch_params(p), f_param_match)
      if (!any(param_match))
        return(NA)
      names(get_patch_params(p)[param_match])
    })))
    params <- params[!is.na(params)]

    # Return the list of corresponding parameter values, one element for each
    # parameter name, each a vector with one element for each elementary component
    # patch of matching type, with each vector element named by the corresponding
    # initial position of the column transformed by that component.
    all_pvs <- purrr::map(params, .f = function(param) {
      pvs <- purrr::map(which(type_match), .f = function(k) {
        get_patch_params(decomposed[[k]])[[param]]
      })

      icps <- purrr::map_int(which(type_match), .f = function(k) {
        final_position <- get_patch_params(decomposed[[k]])[[column_param]]
        terminal_column_position(final_position,
                                 patch = Reduce(compose_patch, rev(decomposed[1:k])),
                                 initial = TRUE)
      })

      names(pvs) <- as.character(icps)
      pvs[sort(names(pvs))]
    })
    names(all_pvs) <- params
    all_pvs
  }

  # Determine the parameter values for the elementary patches (of relevant type)
  # in the target and result.
  target_pvs <- param_values(target)

  # If the corruption does not contain any elementary patches of the given type
  # having real-valued parameters, return NA.
  if (length(target_pvs) == 0)
    return(NA)

  result_pvs <- param_values(result)

  # Assign NAs for any column indices:
  #  - that appear in the target but not the result, or
  #  - whose final column position after application of the result is different
  #    to the final column position after application of the target.
  sink <- purrr::map(intersect(names(target_pvs), names(result_pvs)),
                     .f = function(param) {
                       target_only_icps <- setdiff(names(target_pvs[[param]]),
                                                   names(result_pvs[[param]]))
                       if (length(target_only_icps) != 0)
                         result_pvs[[param]][target_only_icps] <<- NA

                       icps <- names(target_pvs[[param]])
                       is_coincident_icp <- purrr::map_lgl(as.integer(icps), .f = function(i) {
                         terminal_column_position(i, patch = result, FALSE) ==
                           terminal_column_position(i, patch = target, FALSE)
                       })
                       if (!all(is_coincident_icp))
                         result_pvs[[param]][icps[!is_coincident_icp]] <<- NA
                     })

  target_only_params <- setdiff(names(target_pvs), names(result_pvs))

  # Assign NAs for any parameter names that appear in the target but not the result.
  if (length(target_only_params) != 0)
    result_pvs[target_only_params] <- NA

  # Compute the parameter comparison for each parameter and each initial
  # column position. Discard any initial column indices in the result
  # that aren't also in the target.
  ret <- purrr::map(names(target_pvs), .f = function(param) {
    param_comparison <- f_map(names(target_pvs[[param]]), .f = function(i) {
      f_comparison(target_pvs[[param]][[i]], result_pvs[[param]][[i]])
    })
    names(param_comparison) <- names(target_pvs[[param]])
    param_comparison
  })
  names(ret) <- names(target_pvs)
  ret
}
