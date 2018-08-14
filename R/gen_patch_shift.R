#' Generate a shift patch
#'
#' Generates a \code{patch_shift} object whose \code{shift} parameter has been
#' selected with the aim of minimising the mismatch between the specified
#' columns after application of the patch to \code{df1}.
#'
#' Uses the \code{\link{optim}} function to optimise the \code{shift}
#' parameter for the given mismatch method. If the numerical optimisation fails
#' to converge an error is thrown.
#'
#' @param df1
#' A data frame. The column specified in the \code{col1} argument must contain
#' a vector of type \code{double} with at least one non-missing value.
#' @param df2
#' A data frame. The column specified in the \code{col2} argument must contain
#' a vector of type \code{double} with at least one non-missing value.
#' @param mismatch
#' Mismatch method. The default is \code{ks} (Kolmogorov-Smirnov).
#' @param col1
#' A column identifier (integer or string column name) of length 1.
#' @param col2
#' A column identifier (integer or string column name) of length 1. By default
#' this takes the value of \code{col1}.
#'
#' @return A \code{patch_shift} object.
#'
#' @seealso \code{\link{gen_patch_affine}}
#'
#' @export
#'
#' @examples
#' gen_patch_shift(mtcars, mtcars, col1 = "wt", col2 = "qsec")
#'
gen_patch_shift <- function(df1, df2, mismatch = ks, col1, col2 = col1) {

  stopifnot(is_compatible_columns(col1, df1) && length(col1) == 1)
  stopifnot(is_compatible_columns(col2, df2) && length(col2) == 1)

  x1 <- df1[[col1]]
  x2 <- df2[[col2]]

  stopifnot(is.double(x1) && is.double(x2))
  stopifnot(sum(!is.na(x1)) != 0 && sum(!is.na(x2)) != 0)

  # Naive numerical optimisation:
  f <- function(mu) { mismatch(mu + x1, x2) }

  qlow <- min(stats::quantile(x1, probs = 0.25, na.rm = TRUE),
              stats::quantile(x2, probs = 0.25, na.rm = TRUE))
  qhigh <- max(stats::quantile(x1, probs = 0.75, na.rm = TRUE),
               stats::quantile(x2, probs = 0.75, na.rm = TRUE))
  par <- mean(x2, na.rm = TRUE) - mean(x1, na.rm = TRUE)

  optim_par <- stats::optim(par, fn = f, method = "Brent", lower = qlow,
                            upper = qhigh)

  if (optim_par$convergence != 0)
    stop(paste("Optimisation failed with error code", optim_par$convergence))

  shift <- optim_par$par
  patch_shift(col1, shift)
}
