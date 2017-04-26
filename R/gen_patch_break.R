#' Generate a break patch
#'
#' Generates a \code{patch_break} object.
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
#'
#' @return A \code{patch_break} object.
#'
#' @seealso \code{\link{patch_break}}
#'
#' @export
gen_patch_break <- function(df1, col1, df2, col2 = col1) {

  # TODO: the current implementation is temporary and intended only for
  # testing.

  stopifnot(is_compatible_columns(col1, df1))
  stopifnot(is_compatible_columns(col2, df2))

  patch_break(col1)
}
