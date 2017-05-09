#' Total variation distance for two discrete variables (as factors)
#'
#' Compute the total variation distance for samples from two discrete
#' distributions, coded as unordered factors. The result is a number in the
#' (closed) unit interval.
#'
#' @param v1,v2
#' A pair of factors. Both must have at least one non-missing value.
#'
#' @return A number between 0 and 1 inclusive.
#'
#' @export
tv <- function(v1, v2) {

  stopifnot(is.factor(v1) && is.factor(v2))

  if (sum(!is.na(v1)) == 0 || sum(!is.na(v2)) == 0)
    stop("Both arguments must have one or more non-missing values.")

  lev1 <- levels(v1)
  lev2 <- levels(v2)

  # if (length(intersect(lev1, lev2)) == 0)
  #   return(1.0)

  # Align levels of v1 and v2, if necessary.
  if (!identical(lev1, lev2)) {
    lev <- union(lev1, lev2)
    v1 <- factor(v1, levels = lev)
    v2 <- factor(v2, levels = lev)
  }

  nbins <- length(levels(v1))
  t1 <- tabulate(v1, nbins = nbins)
  t2 <- tabulate(v2, nbins = nbins)

  sum(abs(t1/sum(t1) - t2/sum(t2)))/2
}


# OLD:
# NOTE: Factors with disjoint (but equal number of)
# levels are not distinguished. Also, integer vector arguments are not handled
# correctly (always returns 0 since in that case levels() returns NULL)
#
# Total variation for two discrete variables (as factors)
#
# Compute the total variation for two discrete distributions, coded as
# unordered factors. The two distributions are assumed to have common levels in
# the sense that for each level the internal integer representation is the
# same.

# @param f1,f2 An unordered factor or vector of integers. Note that the function
#     \code{tabulate} is used which generates a vector of length equal to the
#     maximum integer in the input (\emph{not} the length of the input).
# @return A number between 0 and 1

# tv_aligned <- function(f1, f2) {
#   nbins <- length(levels(f1))
#   if (nbins != length(levels(f2)))
#     stop("arguments to tv_aligned must have the same number of levels")
#
#   t1 <- tabulate(f1, nbins = nbins)
#   t2 <- tabulate(f2, nbins = nbins)
#
#   0.5 * sum(abs(t1/sum(t1) - t2/sum(t2)))
# }
