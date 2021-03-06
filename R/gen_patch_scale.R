#' Generate a scale patch
#'
#' Generates a \code{patch_scale} object whose \code{scale_factor} parameter has
#' been selected with the aim of minimising the mismatch between the specified
#' columns after application of the patch to \code{df1}.
#'
#' Uses the \code{\link{optimise}} function to optimise the \code{scale_factor}
#' parameter for the given mismatch method.
#'
#' @param df1
#' A data frame. The column specified in the \code{col1} argument must contain
#' a vector of type \code{double} with at least two non-missing values.
#' @param df2
#' A data frame. The column specified in the \code{col2} argument must contain
#' a vector of type \code{double} with at least two non-missing values.
#' @param mismatch
#' Mismatch method. The default is \code{ks} (Kolmogorov-Smirnov).
#' @param col1
#' A column identifier (integer or string column name) of length 1.
#' @param col2
#' A column identifier (integer or string column name) of length 1. By default
#' this takes the value of \code{col1}.
#' @param ...
#' Additional arguments are ignored.
#'
#' @return A \code{patch_scale} object.
#'
#' @seealso \code{\link{gen_patch_affine}} \code{\link{mad}}
#'
#' @export
#'
#' @examples
#' gen_patch_scale(mtcars, mtcars, col1 = "wt", col2 = "qsec")
#'
gen_patch_scale <- function(df1, df2, mismatch = ks, col1, col2 = col1, ...) {

  # Note that the optimal scale factor is never unique, and uniqueness may be
  # lost in two ways:
  # 1. along the 'best step' of the ecdf
  # 2. in the case of multi-modal data there may be multiple 'best steps'. This
  #    may be problematic for numerical optimisation (due to local minima).

  stopifnot(is_compatible_columns(col1, df1) && length(col1) == 1)
  stopifnot(is_compatible_columns(col2, df2) && length(col2) == 1)

  x1 <- df1[[col1]]
  x2 <- df2[[col2]]

  stopifnot(is.double(x1) && is.double(x2))
  stopifnot(sum(!is.na(x1)) != 0 && sum(!is.na(x2)) != 0)

  # Naive numerical optimisation:
  f <- function(lambda) { mismatch(lambda * x1, x2) }

  Q1x1 <- stats::quantile(x1, probs = 0.25, na.rm = TRUE)
  Q3x1 <- stats::quantile(x1, probs = 0.75, na.rm = TRUE)
  Q1x2 <- stats::quantile(x2, probs = 0.25, na.rm = TRUE)
  Q3x2 <- stats::quantile(x2, probs = 0.75, na.rm = TRUE)

  # Most of x2 is negative
  if (Q3x2 < 0) {
    # most of x1 is negative
    if (Q3x1 < 0)
      interval <- c(Q1x2/max(Q3x1, -1), Q3x2/min(Q1x1, -1))
    else {
      # x1 is mixed sign: interval spans the origin
      if (Q1x1 < 0)
        interval <- c(Q1x2/min(Q3x1, 1), Q1x2/max(Q1x1, -1))
      # Most of x1 is positive
      else
        interval <- c(Q1x2/min(Q1x1, 1), Q3x2/max(Q3x1, 1))
    }
  }

  # Most of x2 is positive
  if (Q1x2 > 0) {
    # Most of x1 is negative
    if (Q3x1 < 0)
      interval <- c(Q3x2/max(Q3x1, -1), Q1x2/min(Q1x1, -1))
    else {
      # x1 is mixed sign: interval spans the origin
      if (Q1x1 < 0)
        interval <- c(Q3x2/max(Q1x1, -1), Q3x2/min(Q3x1, 1))
      # Most of x1 is positive
      else
        interval <- c(Q1x2/max(Q3x1, 1), Q3x2/min(Q1x1, 1))
    }
  }

  # x2 is mixed sign: interval spans the origin
  if (Q1x2 < 0 && Q3x2 > 0)
    interval <- c(Q1x2, Q3x2)/min(abs(Q1x1), abs(Q3x1), 1)

  if (Q1x2 == 0) {
    stop("Not yet implemented")
  }

  if (Q3x2 == 0) {
    stop("Not yet implemented")
  }

  optim_par <- stats::optimise(f, interval = interval)

  scale_factor <- optim_par$minimum
  patch_scale(col1, scale_factor)
}
