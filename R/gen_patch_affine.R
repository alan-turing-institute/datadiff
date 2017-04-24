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

  stopifnot(is_compatible_columns(col1, df1) && length(col1) == 1)
  stopifnot(is_compatible_columns(col2, df2) && length(col2) == 1)

  x1 <- df1[[col1]]
  x2 <- df2[[col2]]

  stopifnot(is.double(x1) && is.double(x2))
  stopifnot(sum(!is.na(x1)) != 0 && sum(!is.na(x2)) != 0)

  # Naive numerical optimisation:
  f <- function(params) { diff(params[1] + (params[2] * x1), x2) }
  par <- c(mean(x2, na.rm = TRUE) - mean(x1, na.rm = TRUE), 1)

  par_optim <- stats::optim(par, fn = f)

  if (par_optim$convergence != 0)
    stop("Numerical optimisation failed.")

  p_shift <- patch_shift(cols = col1, shift = par_optim$par[1])
  p_scale <- patch_scale(cols = col1, scale_factor = par_optim$par[2])

  # Given the form of f, the scale must be applied before the shift.
  purrr::compose(p_shift, p_scale)
}
