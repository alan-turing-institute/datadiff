#' Scale a penalty for the two-sample Kolmogorov-Smirnov test
#'
#' @description
#' A penalty is a number associated with a \code{patch} which represents the
#' cost applying the patch. Only patches which reduce the mismatch by an amount
#' in excess of their associated cost are deemed worthwhile. However, the
#' penalty is a fixed number (in the unit interval) whereas the mismatch
#' typically scales with the size of the dataset. Hence, for comparability, the
#' penalty must also be scaled.
#'
#' This function scales the penalty in proportion to the inverse square root of
#' the sample size, so that small mismatches count more the more data
#' points there are.
#' Specifically, the scaling is that appropriate for the
#' \href{https://en.wikipedia.org/wiki/Kolmogorov-Smirnov_test}{two-sample
#' Kolmogorov-Smirnov test} where the confidence threshold \eqn{alpha}
#' corresponds to one minus the penalty.
#'
#' If the \code{penalty} is zero, the return value is also zero.
#'
#' @param penalty A numeric penalty
#' @param nx,ny The lengths of the vectors
#' @return A (numeric) scaled penalty.
#'
#' @seealso \code{\link{linear_scaling}}
#'
#' @export
#'
#' @examples
#' ks_scaling(0.5, nx = 100, ny = 100)
#'
ks_scaling <- function(penalty, nx, ny) {

  if (penalty == 0)
    return(0)
  critical_value <- function(alpha) { sqrt(-log(alpha / 2) / 2) }
  critical_value(1 - penalty) * sqrt((nx + ny) / (nx * ny))
}
