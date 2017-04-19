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
#' A column identifier (integer or string column name) of length 1.
#' @param df2
#' A data frame. The column specified in the \code{col2} argument must contain
#' a vector of type \code{double} with at least two non-missing values.
#' @param col2
#' A column identifier (integer or string column name) of length 1. By default
#' this takes the value of \code{col1}.
#' @param diff
#' Mismatch method. The default is \code{ks} (Kolmogorov-Smirnov).
#'
#' @return A \code{patch_scale} object.
#'
#' @seealso \code{\link{gen_patch_affine}} \code{\link{mad}}
#'
#' @import stats
#' @export
gen_patch_scale <- function(df1, col1, df2, col2 = col1,
                            diff = ks) {

  # Note that the optimal scale factor is never unique, and uniqueness may be
  # lost in multiple ways:
  # - along the 'best step' of the ecdf
  # - in the case of multi-modal data there may be multiple 'best steps'. This
  #   may be problematic for numerical optimisation (due to local minima).

  stopifnot(is_compatible_columns(col1, df1) && length(col1) == 1)
  stopifnot(is_compatible_columns(col2, df2) && length(col2) == 1)

  x1 <- df1[[col1]]
  x2 <- df2[[col2]]

  stopifnot(is.double(x1) && is.double(x2))
  stopifnot(sum(!is.na(x1)) != 0 && sum(!is.na(x2)) != 0)

  # Naive numerical optimisation:
  f <- function(lambda) { diff(lambda * x1, x2) }

  qlow <- quantile(x2, probs = 0.25, na.rm = TRUE) /
    abs(quantile(x1, probs = 0.75, na.rm = TRUE))
  qhigh <- quantile(x2, probs = 0.75, na.rm = TRUE) /
    abs(quantile(x1, probs = 0.25, na.rm = TRUE))
  par <- stats::mad(x2, na.rm = TRUE)/stats::mad(x1, na.rm = TRUE)

  optim_par <- optim(par, fn = f, method = "Brent", lower = qlow, upper = qhigh)

  if (optim_par$convergence != 0)
    stop(paste("Optimisation failed with error code", optim_par$convergence))

  scale_factor <- optim_par$par
  patch_scale(col1, scale_factor)
}
