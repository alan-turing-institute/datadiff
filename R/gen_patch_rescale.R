#' Generate a rescale patch
#'
#' Generates a \code{patch} object representing an affine transformation, whose
#' \code{shift} and \code{scale_factor} parameters have been selected with the
#' aim of minimising the mismatch between the specified columns after
#' application of the patch to \code{df1}.
#'
#' Uses the \code{\link{optim}} function to optimise the affine transformation
#' parameters for the given mismatch method. The numerical methods available to
#' \code{optim} are tried, in the order in which they apper in that function's
#' default \code{method} argument, until convergence is achieved. If none of the
#' methods succeeds an error is thrown.
#'
#' @param df1
#' A data frame. The column specified in the \code{col1} argument must contain
#' a vector of type \code{double} with at least two non-missing values.
#' @param df2
#' A data frame. The column specified in the \code{col2} argument must contain
#' a vector of type \code{double} with at least one two-missing values.
#' @param mismatch
#' Mismatch method. The default is \code{ks} (Kolmogorov-Smirnov).
#' @param col1
#' A column identifier (integer or string column name) of length 1.
#' @param col2
#' A column identifier (integer or string column name) of length 1. By default
#' this takes the value of \code{col1}.
#' @param verbose
#' A logical flag to turn on/off warnings associated with failed optimisation
#' attempts.
#' @param ...
#' Additional arguments are ignored.
#'
#' @return A \code{patch} object of type \code{rescale}.
#'
#' @seealso \code{\link{optim}}
#'
#' @export
#'
#' @examples
#' gen_patch_rescale(mtcars, mtcars, col1 = "wt", col2 = "qsec")
#'
gen_patch_rescale <- function(df1, df2, mismatch = ks, col1, col2 = col1,
                              verbose = FALSE, ...) {

  stopifnot(is_compatible_columns(col1, df1) && length(col1) == 1)
  stopifnot(is_compatible_columns(col2, df2) && length(col2) == 1)

  x1 <- df1[[col1]]
  x2 <- df2[[col2]]

  stopifnot(is.double(x1) && is.double(x2))
  stopifnot(sum(!is.na(x1)) != 0 && sum(!is.na(x2)) != 0)

  # Centre both samples.
  x1_centred <- x1 - mean(x1, na.rm = TRUE)
  x2_centred <- x2 - mean(x2, na.rm = TRUE)

  # Estimate the scale_factor parameter using the centred data.
  par_scale <- estimate_scale_factor(x1_centred, x2_centred, mismatch = mismatch)

  # Scale the x1 data.
  x1_scaled <- par_scale * x1_centred + abs(par_scale) * mean(x1, na.rm = TRUE)

  # Estimate the shift parameter using the scaled data.
  par_shift <- estimate_shift(x1_scaled, x2, mismatch = mismatch)

  patch_rescale(col1, shift = par_shift, scale_factor = par_scale)
}

estimate_scale_factor <- function(x1, x2, mismatch, par = 1) {

  f <- function(lambda) { mismatch(lambda * x1, x2) }

  Q1x1 <- stats::quantile(x1, probs = 0.25, na.rm = TRUE)
  Q3x1 <- stats::quantile(x1, probs = 0.75, na.rm = TRUE)
  Q1x2 <- stats::quantile(x2, probs = 0.25, na.rm = TRUE)
  Q3x2 <- stats::quantile(x2, probs = 0.75, na.rm = TRUE)

  # Most of x2 is negative
  if (Q3x2 < 0) {
    # most of x1 is negative
    if (Q3x1 < 0)
      return(estimate_scale_factor(-x1, -x2, mismatch = mismatch, par = par))
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
  if (Q1x2 < 0 && Q3x2 > 0) {
    # x1 is also mixed sign
    if (Q1x1 < 0 && Q3x1 > 0) {
      Q10x2 <- stats::quantile(x2, probs = 0.1, na.rm = TRUE)
      Q90x2 <- stats::quantile(x2, probs = 0.9, na.rm = TRUE)
      interval <- c(-Q10x2/min(Q1x1, -1), Q90x2/max(Q3x1, 1))
    }
    else
      interval <- c(Q1x2, Q3x2)/min(abs(Q1x1), abs(Q3x1), 1)
  }

  if (Q1x2 == 0) {
    stop("Not yet implemented")
  }

  if (Q3x2 == 0) {
    stop("Not yet implemented")
  }

  if (!(interval[2] > interval[1]))
    warning(paste("Invalid interval for scale factor estimation:",
                  paste(interval, collapse = ", ")))

  optim_par <- stats::optim(par, fn = f, method = "Brent", lower = interval[1],
                            upper = interval[2])

  # Consider non-negative scaling separately.
  if (interval[1] < 0) {
    optim_par_pos <- stats::optim(par, fn = f, method = "Brent", lower = 0,
                              upper = interval[2])
    if (optim_par_pos$convergence == 0) {
      # If the positive interval produces better results, use it.
      if (mismatch(optim_par_pos$par * x1, x2) <= mismatch(optim_par$par * x1, x2) ||
          optim_par_pos$convergence == 0)
        optim_par <- optim_par_pos
    }
  }

  if (optim_par$convergence != 0)
    stop(paste("Scale factor parameter estimation failed with error code",
               optim_par$convergence))

  optim_par$par
}

estimate_shift <- function(x1, x2, mismatch, par = 0) {

  f <- function(mu) { mismatch(mu + x1, x2) }

  Q1x1 <- stats::quantile(x1, probs = 0.25, na.rm = TRUE)
  Q3x1 <- stats::quantile(x1, probs = 0.75, na.rm = TRUE)
  Q1x2 <- stats::quantile(x2, probs = 0.25, na.rm = TRUE)
  Q3x2 <- stats::quantile(x2, probs = 0.75, na.rm = TRUE)

  upper <- max(abs(Q1x1 - Q3x2), abs(Q3x1 - Q1x2))
  lower <- -upper

  optim_par <- stats::optim(par, fn = f, method = "Brent", lower = lower,
                            upper = upper)

  if (optim_par$convergence != 0)
    stop(paste("Shift parameter estimation failed with error code",
               optim_par$convergence))

  optim_par$par
}
