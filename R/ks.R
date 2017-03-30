#' K-S statistic for two numeric vectors or ordered factors.
#'
#' Computes the Kolmogorov-Smirnov statistic for two numeric vectors by
#' comparing their empirical cumulative distribution functions obtained from
#' the \code{\link{ecdf}} function in the \code{stats} package.
#'
#' @param v1,v2
#' A pair of numeric vectors or ordered factors. Both arguments must have at
#' least one non-missing value. In the case of ordered factors the orderings
#' must not conflict and must together determine a unique ordering on the union
#' of the levels.
#'
#' @return A number between 0 and 1 inclusive.
#'
#' @seealso \code{\link{ecdf}}
#'
#' @importFrom stats ecdf
#' @export
ks <- function(v1, v2) {

  # K-S statistic for two ecdf's
  ks_ecdf <- function(e1, e2) {
    knots <- union(knots(e1), knots(e2))
    max(abs(c(e1(knots) - e2(knots))))
  }

  if (is.numeric(v1) && is.numeric(v2))
    return(ks_ecdf(ecdf(v1), ecdf(v2)))

  if (!is.ordered(v1) || !is.ordered(v1))
    stop("Arguments must be numeric vectors or ordered factors.")

  lev1 <- levels(v1)
  lev2 <- levels(v2)

  # Obtain the unique ordering determined by lev1 & lev2, if one exists. (Note
  # that taking the union of lev1 & lev2 does not preserve the ordering.)
  unique_ordering <- function(x, y) {

  # Recursive sub-procedure considers only the first elements, which must either
  # be equal or unique (i.e. not found in the other vector).
    rec_ordering <- function(v, w) {
      if (length(v) == 0 && length(w) == 0)
        return(v)
      if (length(v) == 0)
        return(w)
      if (length(w) == 0)
        return(v)

      if (v[1] == w[1])
        return(c(v[1], rec_ordering(v[-1], w[-1])))

      if (v[1] %in% w) {
        if (w[1] %in% v)
          return(NA)
        return(c(w[1], rec_ordering(v, w[-1])))
      }
      if (w[1] %in% v) {
        if (v[1] %in% w)
          return(NA)
        return(c(v[1], rec_ordering(v[-1], w)))
      }
      NA
    }
    ret <- rec_ordering(x, y)
    if (any(is.na(ret)))
      return(NULL)
    ret
  }

  lev <- unique_ordering(lev1, lev2)

  if (length(lev) == 0)
    stop("Ordered factor levels must determine an ordering.")

  v1 <- factor(v1, levels = lev)
  v2 <- factor(v2, levels = lev)

  ks_ecdf(ecdf(v1), ecdf(v2))
}
