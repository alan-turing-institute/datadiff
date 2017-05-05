#' Generate a transformation patch
#'
#' Generates either an 'affine' \code{patch} object, in case of continuous data,
#' or a 'recode' \code{patch} object in the case of categorical data.
#'
#' @param df1
#' A data frame. The column specified in the \code{col1} argument must contain
#' a vector of type \code{double} with at least two non-missing values.
#' @param df2
#' A data frame. The column specified in the \code{col2} argument must contain
#' a vector of type \code{double} with at least one two-missing values.
#' @param mismatch
#' Mismatch method. The default is \code{ks} (Kolmogorov-Smirnov) for continuous
#' data and \code{tv} (Total variation distance) for categorical data.
#' @param col1
#' A column identifier (integer or string column name) of length 1.
#' @param col2
#' A column identifier (integer or string column name) of length 1. By default
#' this takes the value of \code{col1}.
#' @param verbose
#' A logical flag to turn on/off warnings associated with failed optimisation
#' attempts in the continuous case.
#' @param ...
#' Additional arguments are ignored.
#'
#' @return A \code{patch} object, or \code{NULL} in case of error.
#'
#' @seealso \code{\link{gen_patch_affine}} \code{\link{gen_patch_recode}}
#'
#' @export
gen_patch_transform <- function(df1, df2, mismatch = diffness, col1, col2 = col1,
                                verbose = FALSE, ...) {

  if (is.double(df1[[col1]]))
    gen_patch <- gen_patch_affine
  else
    gen_patch <- gen_patch_recode

  tryCatch(
    gen_patch(df1 = df1, df2 = df2, mismatch = mismatch, col1 = col1, col2 = col2,
              verbose = verbose),
    error = function(e) {
      if (verbose)
        warning(paste0("Patch generator at [", col1, ", ", col2,
                       "] returned error:\n", conditionMessage(e), "\n"))
      NULL
    })
}
