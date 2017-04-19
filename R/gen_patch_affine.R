#' Generate an affine patch
#'
#' Generates a composite 'affine' \code{patch} object, formed as the composition
#' of a \code{patch_shift} and a \code{patch_scale}, whose 'shift' and 'scale
#' factor' parameters hav been selected with the aim of minimising the mismatch
#' between the vector \code{y} and the patch applied to the given data frame.
#'
#' @param df1
#' A data frame. The column specified in the \code{col1} argument must contain
#' a vector of type \code{double} with at least two non-missing values.
#' @param col1
#' A column identifier (integer or string column name) of length 1.
#' @param df2
#' A data frame. The column specified in the \code{col2} argument must contain
#' a vector of type \code{double} with at least one two-missing values.
#' @param col2
#' A column identifier (integer or string column name) of length 1. By default
#' this takes the value of \code{col1}.
#' @param diff
#' Mismatch method. The default is \code{ks} (Kolmogorov-Smirnov).
#'
#' @return A composite \code{patch} object.
#'
#' @seealso \code{\link{gen_patch_shift}} \code{\link{gen_patch_scale}}
#'
#' @export
gen_patch_affine <- function(df1, col1, df2, col2 = col1, diff = ks) {

  p_shift <- gen_patch_shift(df1, col1 = col1, df2 = df2, col2 = col2,
                             diff = diff)
  p_scale <- gen_patch_scale(p_shift(df1), col1 = col1, df2 = df2, col2 = col2,
                             diff = diff)

  purrr::compose(p_scale, p_shift)

  # old:
  # p_scale <- gen_patch_scale(df1, col1 = col1, df2 = df2, col2 = col2)
  # scale_factor <- get_patch_params(p_scale)[["scale_factor"]]
  #
  # # Take into account the scale factor when determining the shift, so the sample
  # # statistics for the transformed and target data match.
  # if (shift_first)
  #   p <- patch_scale(col2, scale_factor = 1/scale_factor)
  # else
  #   p <- patch_shift(col2, shift = (1 - scale_factor) * mean(df1[[col1]]))
  #
  # p_shift <- gen_patch_shift(df1, col1 = col1, df2 = p(df2), col2 = col2)
  # ifelse(shift_first,
  #        yes = purrr::compose(p_scale, p_shift),
  #        no = purrr::compose(p_shift, p_scale))
}
