#' Perform a column-wise two-sample Kolmogorov-Smirnov test
#'
#' Performs a \href{https://en.wikipedia.org/wiki/Kolmogorov-Smirnov_test}{two-sample
#' Kolmogorov-Smirnov test} with a given confidence level \eqn{alpha}.
#'
#' @param df1,df2
#' A pair of data frames.
#' @param alpha
#' A scalar double value specifying a confidence level.
#'
#' @return A logical vector of length equal to \code{min(ncol(df1), ncol(df2))}
#' indicating whether the null hypothesis was rejected (\code{TRUE}) or not
#' (\code{FALSE}) at the given confidence level.
#'
#' @seealso \code{\link{ks}}
#'
#' @export
columnwise_ks <- function(df1, df2, alpha) { #, cross_product = FALSE) {

  stopifnot(length(alpha) == 1)
  stopifnot(alpha > 0)
  stopifnot(alpha < 1)

  ### TODO:
  # - add an argument to optionally perform a cross-product columnpair-wise
  # test, in which case we would 'not reject' only if there is a unique column
  # pair (whose indices need not be identical) for which the null is not rejected
  # at the chosen level.
  cols <- 1:min(ncol(df1), ncol(df2))

  is_target_col <- purrr::map_lgl(cols, .f = function(i) {
    (is.numeric(df1[[i]]) && is.numeric(df2[[i]])) ||
      (is.ordered(df1[[i]]) && is.ordered(df2[[i]]))
  })

  if (!any(is_target_col))
    return(rep(NA, times = length(cols)))

  ks_stats <- purrr::map_dbl(cols, .f = function(i) {
    if (!is_target_col[i])
      return(NA)
    ks(df1[[i]], df2[[i]])
  })

  # K-S test predicate.
  predicate <- function(D, nx, ny) { D > ks_scaling(1 - alpha, nx = nx, ny = ny) }

  purrr::map_lgl(cols, .f = function(i) {
    if (!is_target_col[i])
      return(as.logical(NA))
    nx <- sum(!is.na(df1[[i]]))
    ny <- sum(!is.na(df2[[i]]))
    predicate(ks_stats[i], nx = nx, ny = ny)
  })
}
