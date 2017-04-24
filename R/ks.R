#' K-S statistic
#'
#' Computes the two-sample Kolmogorov-Smirnov statistic.
#'
#' @param v1,v2
#' A pair of numeric vectors or ordered factors.
#'
#' @return A number between 0 and 1 inclusive.
#'
#' @export
ks <- function(v1, v2) UseMethod("ks")

#' K-S statistic for two ecdf's
#'
#' Internal function to calculate the K-S statistic for two empirical
#' cumulative distribution functions.
#'
#' @param e1,e2
#' A pair of ecdf objects.
#'
#' @return A number between 0 and 1 inclusive.
ks_ecdf <- function(e1, e2) {
  knots <- union(knots(e1), knots(e2))
  max(abs(c(e1(knots) - e2(knots))))
}

#' K-S statistic for two numeric vectors
#'
#' Computes the Kolmogorov-Smirnov statistic for two numeric vectors by
#' comparing their empirical cumulative distribution functions obtained from
#' the \code{\link{ecdf}} function.
#'
#' @param v1,v2
#' A pair of numeric vectors. Both arguments must have at
#' least one non-missing value.
#'
#' @return A number between 0 and 1 inclusive.
#'
#' @seealso \code{\link{ecdf}}
#'
#' @export
ks.numeric <- function(v1, v2) {

  stopifnot(is.numeric(v2))
  ks_ecdf(stats::ecdf(v1), stats::ecdf(v2))
}

#' K-S statistic for two ordered factors.
#'
#' Computes the Kolmogorov-Smirnov statistic for two ordered factors by
#' first determining a unique ordering on the union of the factor levels, if
#' one exists, and then comparing the empirical cumulative distribution
#' functions obtained from the \code{\link{ecdf}} function.
#'
#' @param v1,v2
#' A pair of ordered factors. Both arguments must have at least one non-missing
#' value. The orderings must not conflict and must together determine a unique
#' ordering on the union of the levels.
#'
#' @return A number between 0 and 1 inclusive.
#'
#' @seealso \code{\link{ecdf}}
#'
#' @export
ks.ordered <- function(v1, v2) {

  stopifnot(is.ordered(v2))

  lev1 <- levels(v1)
  lev2 <- levels(v2)

  # Obtain the unique ordering determined by lev1 & lev2, if one exists. (Note
  # that taking the union of lev1 & lev2 does not preserve the ordering.)
  unique_ordering <- function(x, y) {

  # Recursive sub-procedure considers only the first elements, which must either
  # be equal or unique (i.e. found in one but not both vectors).
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

  ks_ecdf(stats::ecdf(v1), stats::ecdf(v2))
}
