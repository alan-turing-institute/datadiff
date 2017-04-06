#' Generate a shift patch
#'
#' Generates a \code{patch_shift} object whose 'shift' parameter has been
#' selected with the aim of minimising the mismatch between the specified
#' columns after application of the patch to \code{df1}.
#'
#' @param df1
#' A data frame. The column specified in the \code{col1} argument must contain
#' a vector of type \code{double} with at least one non-missing value.
#' @param col1
#' A column identifier (integer or string column name) of length 1.
#' @param df2
#' A data frame. The column specified in the \code{col2} argument must contain
#' a vector of type \code{double} with at least one non-missing value.
#' @param col2
#' A column identifier (integer or string column name) of length 1. By default
#' this takes the value of \code{col1}.
#' @param ...
#' Additional arguments passed to the \code{mean} function.
#'
#' @return A \code{patch_shift} object.
#'
#' @seealso \code{\link{gen_patch_affine}}
#'
#' @import stats
#' @export
gen_patch_shift <- function(df1, col1, df2, col2 = col1, ...) {

  stopifnot(is_compatible_columns(col1, df1) && length(col1) == 1)
  stopifnot(is_compatible_columns(col2, df2) && length(col2) == 1)

  x <- df1[[col1]]
  y <- df2[[col2]]

  stopifnot(is.double(x) && is.double(y))
  stopifnot(sum(!is.na(x)) != 0 && sum(!is.na(y)) != 0)

  patch_shift(col1, shift = mean(y, ...) - mean(x, ...))
}
