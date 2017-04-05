#' Generate a scale patch
#'
#' Generates a \code{patch_scale} object whose 'scale factor' parameter has been
#' selected with the aim of minimising the mismatch between the specified
#' columns after application of the patch to \code{df1}.
#'
#' @param df1
#' A data frame. The column specified in the \code{col1} argument must contain
#' a vector of type \code{double} with at least two non-missing values.
#' @param col1
#' A column identifier (integer or string column name) with length 1.
#' @param df2
#' A data frame. The column specified in the \code{col2} argument must contain
#' a vector of type \code{double} with at least two non-missing values.
#' @param col2
#' A column identifier (integer or string column name) with length 1. By default
#' this takes the value of \code{col1}.
#' @param robust
#' A logical flag. If \code{TRUE} (the default) the median absolute deviation is
#' used as a robust measure of statistical dispersion. Otherwise the standard
#' deviation is used.
#' @param ...
#' Additional arguments passed to the function used to calculate the statistical
#' dispersion (i.e. \code{\link{mad}} if \code{robust} is \code{TRUE}, otherwise
#' \code{\link{sd}}).
#'
#' @return A \code{patch_scale} object.
#'
#' @seealso \code{\link{gen_patch_affine}} \code{\link{mad}}
#'
#' @import stats
#' @export
gen_patch_scale <- function(df1, col1, df2, col2 = col1,
                            robust = TRUE, ...) {

  stopifnot(is_compatible_columns(col1, df1) && length(col1) == 1)
  stopifnot(is_compatible_columns(col2, df2) && length(col2) == 1)

  x <- df1[[col1]]
  y <- df2[[col2]]

  stopifnot(is.double(x) && is.double(y))
  stopifnot(sum(!is.na(x)) != 0 && sum(!is.na(y)) != 0)

  f <- ifelse(robust, yes = stats::mad, no = stats::sd)

  fx <- f(x, ...)
  fy <- f(y, ...)
  if (identical(fx, 0) || identical(fy, 0))
     stop("Zero dispersion.")

  patch_scale(col1, scale_factor = fy/fx)
}
