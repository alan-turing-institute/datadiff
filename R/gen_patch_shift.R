#' Generate a shift patch
#'
#' Generates a \code{patch_shift} object whose 'shift' parameter has been
#' selected with the aim of minimising the mismatch between the vector \code{y}
#' and the patch applied to the given data frame.
#'
#' @param df
#' A data frame. The column specified in the \code{cols} argument must contain a
#' vector of type \code{double} with at least one non-missing value.
#' @param cols
#' A column identifier (integer or string column name) with length 1.
#' @param y
#' A 'target' vector of type \code{double} with least one non-missing value.
#' @param ...
#' Additional arguments passed to the \code{mean} function.
#'
#' @return A \code{patch_shift} object.
#'
#' @seealso \code{\link{gen_patch_affine}}
#'
#' @import stats
#' @export
gen_patch_shift <- function(df, cols, y) {

  stopifnot(is_valid_columns(cols) && length(cols) == 1)

  x <- df[[cols]]
  stopifnot(is.double(x) && is.double(y))
  stopifnot(sum(!is.na(x)) != 0 && sum(!is.na(y)) != 0)

  patch_shift(cols, shift = mean(y) - mean(x))
}
