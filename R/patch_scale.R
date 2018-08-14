#' \code{patch_scale} S3 class constructor
#'
#' @description
#' S3 class \code{patch_scale} which extends the \code{patch} and
#' \code{function} classes to represent a transformation of a tabular dataset
#' by rescaling numerical data in one or more columns.
#'
#' @param cols
#' A vector of column identifiers.
#' @param scale_factor
#' A numeric scalar.
#'
#' @return A \code{patch_scale} object.
#'
#' @export
#'
#' @seealso \code{\link{is_valid_columns}} \code{\link{is_compatible_columns}}
#'
#' @examples
#' head(mtcars)
#' p <- patch_scale(c(1L, 3L), scale_factor = 2)
#'
#' # The following are equivalent:
#' head(apply_patch(mtcars, p))
#' head(p(mtcars))
#'
#' # Attempting to apply a patch to an incompatible data frame throws an error.
#' \dontrun{
#' p <- patch_scale(c(1L, 22L), scale_factor = 2)
#' p(mtcars)
#' }
patch_scale <- function(cols, scale_factor) {

  # Check the given parameters are appropriate for the delete patch type.
  stopifnot(is_valid_columns(cols))
  stopifnot(length(scale_factor) == 1 && is.numeric(scale_factor))

  # Construct the patch object as a closure.
  obj <- function(df) {

    # Check for compatability between the data frame and the parameters.
    stopifnot(is_compatible_columns(cols, df))
    stopifnot(all(purrr::map_lgl(df[cols], is.numeric)))

    # Transform the data frame according to the parameters.
    df[cols] <- scale_factor * df[cols]
    df
  }

  class(obj) <- c("patch_scale", "patch", "function")
  obj
}

#' Randomly sample from a distribution of scale patches
#'
#' @param df
#' A data frame
#' @param rdist
#' A function for randomly generating the scale factor. Must
#' contain an argument \code{n} for the number of samples. Defaults to the Normal
#' distribution function \code{rnorm} with unit mean and standard deviation.
#' @param mean
#' (Optional) A number specifying the \code{mean} parameter in the Normal
#' distribution function which is the default value for \code{rdist}. Note that
#' this argument is ignored unless \code{rdist} takes its default value. Defaults
#' to 1.
#' @param exclude_cols
#' An integer vector of column indices to be excluded from the set of possible
#' target columns for the returned patch.
#' @param seed
#' A random seed.
#' @param ...
#' Additional arguments passed to the \code{rdist} function.
#'
#' @return A scale patch with randomly sampled parameters.
#'
#' @export
#'
#' @examples
#' # By default, the scale parameter is sampled from a standard Normal distribution:
#' sample_patch_scale(mtcars)
#' sample_patch_scale(mtcars, mean = 2)
#'
sample_patch_scale <- function(df,
                               rdist = purrr::partial(stats::rnorm, mean = mean),
                               mean = 1, exclude_cols = integer(0), seed, ...) {

  if (!missing(seed))
    set.seed(seed)

  condition <- function(x) { is.double(x) && !all(as.integer(x) == x) }
  cols <- sample_cols(df, condition = condition, exclude_cols = exclude_cols,
                      size = 1)
  patch_scale(cols, scale_factor = rdist(n = 1, ...))
}


