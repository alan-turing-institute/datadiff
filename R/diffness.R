#' Compute the mismatch between two datasets
#'
#' \code{diffness} is a generic function: the type of the first argument
#' determines which method is called.
#'
#' @param x,y
#' A pair of datasets
#' @param scale
#' A logical flag. If \code{TRUE} (the default) the result is scaled by the
#' size of the data.
#' @param ...
#' Additional arguments passed to methods.
#'
#' @return A non-negative number quantifying the mismatch between the two
#' datasets.
#'
#' @export
diffness <- function(x, y, scale, ...) UseMethod("diffness")

#' Compute the mismatch between two data frames
#'
#' @param x,y
#' A pair of datasets
#' @param scale
#' A logical flag. If \code{TRUE} (the default) the result is scaled by the
#' number of rows in the data.
#' @param col_diff
#' A numeric scalar specifying the additional mismatch per column when \code{x}
#' and \code{y} have different numbers of columns. Defaults to 1.
#' @param ...
#' Additional arguments passed to methods.
#'
#' @export
diffness.data.frame <- function(x, y, scale = TRUE, col_diff = 1, ...) {

  stopifnot(is.data.frame(y))
  stopifnot(length(x) > 0 && length(y) > 0)

  if (length(x) != length(y)) {
    stopifnot(is.numeric(col_diff) && length(col_diff) == 1)
    n <- min(length(x), length(y))
    m <- max(length(x), length(y))

    col_diff_add <- col_diff * (m - n)
    if (scale)
      col_diff_add <- col_diff_add * diffness_scale(nrow(x), nrow(y))

    return(diffness(x[1:n], y[1:n], scale = scale, ...) + col_diff_add)
  }

  sum(purrr::map2_dbl(x, y, .f = diffness, scale = scale, ...))
}

# NOTE: numeric is equivalent to (double OR integer). Since we want to exclude
# integer (i.e. treat that case as discrete) it makes sense to have
# diffness.integer & diffness.double, and omit diffness.numeric.

#' Compute the mismatch between two continuous numeric vectors
#'
#' @param x,y
#' A pair of vectors of type \code{double}.
#' @param scale
#' A logical flag. If \code{TRUE} (the default) the result is scaled by the
#' length of the data.
#' @param diff
#' Mismatch method. The default is \code{ks} (Kolmogorov-Smirnov) for continuous
#' numeric data.
#' @param ...
#' Additional arguments are ignored.
#'
#' @export
diffness.double <- function(x, y, scale = TRUE, diff = ks, ...) {
  stopifnot(is.vector(y))

  if (!is.double(y))
    return(ifelse(scale, yes = +Inf, no = 1))

  ret <- diff(x, y)
  if (!scale)
    return(ret)
  ret * diffness_scale(length(x), length(y))
}

#' Compute the mismatch between two integer vectors
#'
#' By default, integer vectors are treated as ordered categorical data. To
#' ignore ordering, set the \code{diff} argument to \code{tv} (total variation
#' distance).
#'
#' The mismatch between an integer vector and a non-integer numeric vector is
#' always +Inf.
#'
#' @param x,y
#' A pair of vectors of type \code{integer}.
#' @param scale
#' A logical flag. If \code{TRUE} (the default) the result is scaled by the
#' length of the data.
#' @param diff
#' Mismatch method. The default is \code{ks} (Kolmogorov-Smirnov) for integer
#' data.
#' @param ...
#' Additional arguments are ignored.
#'
#' @export
diffness.integer <- function(x, y, scale = TRUE, diff = ks, ...) {
  stopifnot(is.vector(y))

  if (!is.integer(y)) return(+Inf)

  # Note that, when diff = ks (the default), the following call to diff(x, y)
  # produces the same result as if we were to explicitly treat integers as
  # ordered categorical data by calling:
  # diffness(as.ordered(x), as.ordered(y), diff = ks).
  ret <- diff(x, y)
  if (!scale)
    return(ret)
  ret * diffness_scale(length(x), length(y))
}

#' Compute the mismatch between two vectors of ordered categorical data
#'
#' @param x,y
#' A pair of ordered factors.
#' @param scale
#' A logical flag. If \code{TRUE} (the default) the result is scaled by the
#' length of the data.
#' @param diff
#' Mismatch method. The default is \code{ks} (Kolmogorov-Smirnov) for ordered
#' categorical data.
#' @param ...
#' Additional arguments are ignored.
#'
#' @export
diffness.ordered <- function(x, y, scale = TRUE, diff = ks, ...) {
  stopifnot(is.factor(y))

  # Ordered discrete data are, by default, compared using ks, rather
  # than tv (although both make sense), since this takes into account the
  # separation implied by the ordering. See test-diffness.R.

  # Note: in contrast to other diffness methods, we do not test that y is
  # ordered (and return 1.0 if not) as this might give unexpected results
  # (and ks will give an error in that case).
  ret <- diff(x, y)
  if (!scale)
    return(ret)
  ret * diffness_scale(length(x), length(y))
}

#' Compute the mismatch between two vectors of unordered categorical data
#'
#' @param x,y
#' A pair of unordered factors.
#' @param scale
#' A logical flag. If \code{TRUE} (the default) the result is scaled by the
#' length of the data.
#' @param diff
#' Mismatch method. The default is \code{tv} (total variation distance) for
#' unordered categorical data.
#' @param ...
#' Additional arguments are ignored.
#'
#' @export
diffness.factor <- function(x, y, scale = TRUE, diff = tv, ...) {
  stopifnot(is.factor(y))

  if (!is.factor(y)) return(+Inf)

  ret <- diff(x, y)
  if (!scale)
    return(ret)
  ret * diffness_scale(length(x), length(y))
}

#' Compute the mismatch between two character vectors
#'
#' Character vectors are treated as unordered categorical data.
#'
#' @param x,y
#' A pair of vectors of type \code{character}.
#' @param scale
#' A logical flag. If \code{TRUE} (the default) the result is scaled by the
#' length of the data.
#' @param diff
#' Mismatch method. The default is \code{tv} (total variation distance) for
#' unordered categorical data.
#' @param ...
#' Additional arguments are ignored.
#'
#' @export
diffness.character <- function(x, y, scale = TRUE, diff = tv, ...) {
  stopifnot(is.vector(y))

  if (!is.character(y)) return(+Inf)

  diffness(as.factor(x), as.factor(y), diff = diff, scale = scale)
}

#' Compute the mismatch between two logical vectors
#'
#' Logical vectors are treated as unordered categorical data.
#'
#' @param x,y
#' A pair of vectors of type \code{logical}.
#' @param scale
#' A logical flag. If \code{TRUE} (the default) the result is scaled by the
#' length of the data.
#' @param diff
#' Mismatch method. The default is \code{tv} (total variation distance) for
#' unordered categorical data.
#' @param ...
#' Additional arguments are ignored.
#'
#' @export
diffness.logical <- function(x, y, scale = TRUE, diff = tv, ...) {
  stopifnot(is.vector(y))

  if (!is.logical(y)) return(+Inf)

  diffness(as.factor(x), as.factor(y), diff = diff, scale = scale)
}


#' Compute "mismatch scale factor"
#'
#' The scale factor increases the diffness in proportion to the square root of
#' the number of rows. This means that, when comparing against a "patch cost",
#' small diffnesses count more the more data points there are.
#'
#' @param nx,ny The lengths of the vectors
#' @return A number
#'
diffness_scale <- function(nx, ny) {
    sqrt(nx * ny / (nx + ny))
}


# TPH: Since the following functions aren't implementations of a generic
# function, the type of v1 is not known, but is assumed. For this reason
# I've moved the logic to the preceding generic methods.

## ----------------------------------------------------------------------------
## These functions compute the diffness for different types.

# diffness_continuous <- function(v1, v2, diff = ks) {
#   if (!is_continuous(v2)) return(1.0)
#
#   diff(v1, v2)
# }
#
# diffness_ordered <- function(v1, v2, diff = ks) {
#   if (!is_ordered(v2)) return(1.0)
#   stop("Not implemented yet")
# }
#
# diffness_categorical <- function(v1, v2, diff = tv) {
#   if (!is_categorical(v2)) return(1.0)
#
#   diff(v1, v2)
# }
