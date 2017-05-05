#' Generate an affine patch
#'
#' Generates a composite 'affine' \code{patch} object, formed as the composition
#' of a \code{patch_shift} and a \code{patch_scale}, whose 'shift' and 'scale
#' factor' parameters hav been selected with the aim of minimising the mismatch
#' between the vector \code{y} and the patch applied to the given data frame.
#'
#' Uses the \code{\link{optim}} function to optimise the affine transformation
#' parameters for the given mismatch (or 'mismatch') method, which defaults to
#' \code{\link{ks}} (Kolmogorov-Smirnov). The numerical methods available to
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
#' @return An 'affine' \code{patch} object (consisting of the composition of a
#' shift and scale patch) with an attribute containing the \code{mismatch} measure
#' used to generate it.
#'
#' @seealso \code{\link{optim}}
#'
#' @export
gen_patch_affine <- function(df1, df2, mismatch = ks, col1, col2 = col1,
                             verbose = FALSE, ...) {

  stopifnot(is_compatible_columns(col1, df1) && length(col1) == 1)
  stopifnot(is_compatible_columns(col2, df2) && length(col2) == 1)

  x1 <- df1[[col1]]
  x2 <- df2[[col2]]

  stopifnot(is.double(x1) && is.double(x2))
  stopifnot(sum(!is.na(x1)) != 0 && sum(!is.na(x2)) != 0)

  # Naive numerical optimisation:
  f <- function(params) { mismatch(params[1] + (params[2] * x1), x2) }
  par <- c(mean(x2, na.rm = TRUE) - mean(x1, na.rm = TRUE), 1)

  # Try all applicable numerical methods, as necessary.
  # Method "Brent" is for one-dimensional problems only.
  optim_methods <- setdiff(eval(formals(stats::optim)[["method"]]), "Brent")
  i <- 1
  par_optim <- stats::optim(par, fn = f, method = optim_methods[i])

  # There is no do-while loop in R.
  while(par_optim[["convergence"]] != 0 && i != length(optim_methods) + 1) {

    if (verbose) {
      msg <- paste("Optimisation attempt with method", optim_methods[i],
                   "failed with code:", par_optim[["convergence"]])
      if (length(par_optim[["message"]]) != 0)
        msg <- paste(msg, "and message:", par_optim[["message"]])
      warning(msg)
    }

    i <- i + 1
    par_optim <- stats::optim(par, fn = f, method = optim_methods[i])
  }

  if (par_optim[["convergence"]] != 0)
    stop("Numerical optimisation failed.")

  p_shift <- patch_shift(cols = col1, shift = par_optim[["par"]][1])
  p_scale <- patch_scale(cols = col1, scale_factor = par_optim[["par"]][2])

  # Given the form of f, the scale must be applied before the shift.
  ret <- compose_patch(p_shift, p_scale)
  attr(ret, which = "mismatch") <- mismatch
  ret
}
