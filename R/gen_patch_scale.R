#' Generate a scale patch
#'
#' Generates a \code{patch_scale} object whose 'scale factor' parameter has been
#' selected with the aim of minimising the mismatch between the vector \code{y}
#' and the patch applied to the given data frame.
#'
#' @param df
#' A data frame. The column specified in the \code{cols} argument must contain a
#' vector of type \code{double} with at least two non-missing values.
#' @param cols
#' A column identifier (integer or string column name) with length 1.
#' @param y
#' A 'target' vector of type \code{double} with least two non-missing values.
#' @param robust
#' A logical flag. If \code{TRUE} (the default) the median absolute deviation is
#' used as a robust measure of statistical dispersion. Otherwise the standard
#' deviation is used.
#' @param ...
#' Additional arguments passed to the function used to calculate the statistical
#' dispersion (\code{\link{mad}} if \code{robust} is \code{TRUE}, otherwise
#' \code{\link{sd}}).
#'
#' @return A \code{patch_scale} object.
#'
#' @seealso \code{\link{gen_patch_affine}} \code{\link{mad}}
#'
#' @import stats
#' @export
gen_patch_scale <- function(df, cols, y, robust = TRUE, ...) {

  stopifnot(is_valid_columns(cols) && length(cols) == 1)

  x <- df[[cols]]
  stopifnot(is.double(x) && is.double(y))
  stopifnot(sum(!is.na(x)) != 0 && sum(!is.na(y)) != 0)

  f <- ifelse(robust, yes = stats::mad, no = stats::sd)

  fx <- f(x, ...)
  fy <- f(y, ...)
  if (identical(fx, 0) || identical(fy, 0))
     stop("Zero dispersion.")

  patch_scale(cols, scale_factor = fy/fx)
}
