#' \code{patch_shift} S3 class constructor.
#'
#' @description
#' S3 class \code{patch_shift} which extends the \code{patch} and
#' \code{function} classes to represent a transformation of a tabular dataset
#' by shifting (adding a constant to) numerical data in one or more columns.
#'
#' @param cols
#' A vector of column identifiers.
#' @param shift
#' A numeric scalar.
#'
#' @return A \code{patch_shift} object.
#'
#' @export
#'
#' @seealso \code{\link{is_valid_columns}} \code{\link{is_compatible_columns}}
#'
#' @examples
#' head(mtcars)
#' p <- patch_shift(c(1L, 3L), shift = 2)
#'
#' # The following are equivalent:
#' head(apply_patch(mtcars, p))
#' head(p(mtcars))
#'
#' # Attempting to apply a patch to an incompatible data frame throws an error.
#' \dontrun{
#' p <- patch_shift(c(1L, 22L), shift = 2)
#' p(mtcars)
#' }
patch_shift <- function(cols, shift) {

  # Check the given parameters are appropriate for the delete patch type.
  stopifnot(is_valid_columns(cols))
  stopifnot(length(shift) == 1 && is.numeric(shift))

  # Construct the patch object as a closure.
  obj <- function(df) {

    # Check for compatability between the data frame and the parameters.
    stopifnot(is_compatible_columns(cols, df))
    stopifnot(all(purrr::map_lgl(df[cols], is.numeric)))

    # Transform the data frame according to the parameters.
    df[cols] <- shift + df[cols]
    df
  }

  class(obj) <- c("patch_shift", "patch", "function")
  obj
}

#' Randomly sample from a distribution of shift patches
#'
#' @param df
#' A data frame
#' @param rdist
#' A function for randomly generating the shift parameter. Must
#' contain an argument \code{n} for the number of samples. Defaults to the
#' standard Normal distribution function \code{rnorm}.
#' @param relative_shift
#' (Optional) A number specifying a relative shift size. If non-\code{NULL}, the
#' \code{rdist} argument is ignored and the shift is sampled from the Normal
#' distribution with mean (and standard deviation, respectively) given by
#' \eqn{relative_shift} multiplied by the median (respectively standard deviation)
#' of the values in the selected column of \code{df} (ignoring NAs). The
#' \code{relative_shift} argument defaults to \code{NULL} to produce a shift
#' which is absolute, not relative, and is sampled using the \code{rdist}
#' function.
#' @param exclude_cols
#' An integer vector of column indices to be excluded from the set of possible
#' target columns for the returned patch.
#' @param seed
#' A random seed.
#' @param ...
#' Additional arguments passed to the \code{rdist} function.
#'
#' @return A shift patch with randomly sampled parameters.
#'
#' @export
#'
#' @examples
#' # By default, the shift parameter is sampled from a standard Normal distribution:
#' sample_patch_shift(mtcars)
#' sample_patch_shift(mtcars, mean = 2)
#'
sample_patch_shift <- function(df,
                               rdist = stats::rnorm,
                               relative_shift = NULL,
                               exclude_cols = integer(0),
                               seed, ...) {

  if (!missing(seed))
    set.seed(seed)

  condition <- function(x) { is.double(x) && !all(as.integer(x) == x) }
  cols <- sample_cols(df, condition = condition, exclude_cols = exclude_cols,
                      size = 1)

  if (!is.null(relative_shift)) {
    stopifnot(is.numeric(relative_shift))
    stopifnot(length(relative_shift) == 1)
    mean <- relative_shift * stats::median(df[[cols]], na.rm = TRUE)
    sd <- relative_shift * stats::sd(df[[cols]], na.rm = TRUE)
    rdist <- purrr::partial(stats::rnorm, mean = mean, sd = sd)
  }

  patch_shift(cols, shift = rdist(n = 1, ...))
}
