#' Generate an affine patch
#'
#' Generates a composite 'affine' \code{patch} object, formed as the composition
#' of a \code{patch_shift} and a \code{patch_scale}, whose 'shift' and 'scale
#' factor' parameters hav been selected with the aim of minimising the mismatch
#' between the vector \code{y} and the patch applied to the given data frame.
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
#' Additional arguments passed to the .
#'
#' @return A composite \code{patch} object.
#'
#' @seealso \code{\link{gen_patch_shift}} \code{\link{gen_patch_scale}}
#'
#' @import purrr
#' @export
gen_patch_affine <- function(df, cols, y, robust = TRUE, ...) {

  stopifnot(is_valid_columns(cols) && length(cols) == 1)

  p2 <- gen_patch_scale(df, cols = cols, y = y, robust = robust, ...)

  # Take into account the scale factor when determining the shift so the sample
  # statistics for the transformed and target data match.
  scale_factor <- get_patch_params(p2)[["scale_factor"]]
  p1 <- gen_patch_shift(df, cols = cols, y = y/scale_factor)

  purrr::compose(p2, p1)
}
