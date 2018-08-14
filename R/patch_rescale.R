#' \code{patch_rescale} S3 class constructor
#'
#' @description
#' S3 class \code{patch_rescale} which extends the \code{patch} and
#' \code{function} classes to represent a transformation of a tabular dataset
#' by applying an affine rescaling to numerical data in one or more columns.
#'
#' @param cols
#' A vector of column identifiers.
#' @param shift
#' A numeric scalar. Defaults to zero.
#' @param scale_factor
#' A numeric scalar. Defaults to one.
#'
#' @return A \code{patch_rescale} object.
#'
#' @export
#'
#' @seealso \code{\link{is_valid_columns}} \code{\link{is_compatible_columns}}
#'
#' @examples
#' head(mtcars)
#' p <- patch_rescale(c(1L, 3L), shift = 10, scale_factor = 2)
#' p
#'
#' # The following are equivalent:
#' head(apply_patch(mtcars, p))
#' head(p(mtcars))
#'
#' # Attempting to apply a patch to an incompatible data frame throws an error.
#' \dontrun{
#' p <- patch_rescale(c(1L, 22L), shift = 10, scale_factor = 2)
#' p(mtcars)
#' }
patch_rescale <- function(cols, shift = 0, scale_factor = 1) {

  # Check the given parameters are appropriate for the delete patch type.
  stopifnot(is_valid_columns(cols))
  stopifnot(length(scale_factor) == 1 && is.numeric(scale_factor))
  stopifnot(length(shift) == 1 && is.numeric(shift))

  # Construct the patch object as a closure.
  obj <- function(df) {

    # Check for compatability between the data frame and the parameters.
    stopifnot(is_compatible_columns(cols, df))
    stopifnot(all(purrr::map_lgl(df[cols], is.numeric)))

    # Transform the data frame according to the parameters.
    df[cols] <- shift + scale_factor * df[cols]
    df
  }

  class(obj) <- c("patch_rescale", "patch", "function")
  obj
}

#' Randomly sample from a distribution of rescale patches
#'
#' @param df
#' A data frame
#' @param rdist_shift
#' A function for randomly generating the shift. Must contain an argument
#' \code{n} for the number of samples. Defaults to the Normal distribution
#' function \code{rnorm} with zero mean and standard deviation.
#' @param mean_shift
#' (Optional) A number specifying the \code{mean} parameter in the Normal
#' distribution function which is the default value for \code{rdist_shift}. Note
#' that this argument is ignored unless \code{rdist_shift} takes its default
#' value. Defaults to 0.
#' @param relative_shift
#' (Optional) A number specifying a relative shift size. If non-\code{NULL}, the
#' \code{rdist} and \code{mean_shift} arguments are ignored and the shift is
#' sampled from the Normal distribution with mean (and standard deviation,
#' resp.) given by \code{relative_shift} multiplied by the median
#' (resp. standard deviation) of the values in the selected column
#' of \code{df} (ignoring \code{NA}s). The \code{relative_shift} argument defaults
#' to \code{NULL} to produce a shift which is absolute, not relative.
#' @param rdist_scale
#' A function for randomly generating the scale parameter. Must contain an
#' argument \code{n} for the number of samples. Defaults to the standard Normal
#' distribution function \code{rnorm}.
#' @param mean_scale
#' (Optional) A number specifying the \code{mean} parameter in the Normal
#' distribution function which is the default value for \code{rdist_scale}. Note
#' that this argument is ignored unless \code{rdist_scale} takes its default
#' value. Defaults to 1.
#' @param exclude_cols
#' An integer vector of column indices to be excluded from the set of possible
#' target columns for the returned patch.
#' @param seed
#' A random seed.
#'
#' @return A rescale patch with randomly sampled parameters.
#'
#' @export
#'
#' @examples
#'
#' # By default, parameter values are sampled from a standard Normal distribution:
#' sample_patch_rescale(mtcars)
#' sample_patch_rescale(mtcars, mean_shift = 10, mean_scale = 2)
#'
sample_patch_rescale <- function(df,
                               rdist_shift =
                                 purrr::partial(stats::rnorm, mean = mean_shift),
                               mean_shift = 0,
                               relative_shift = NULL,
                               rdist_scale =
                                 purrr::partial(stats::rnorm, mean = mean_scale),
                               mean_scale = 1,
                               exclude_cols = integer(0),
                               seed) {

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
    rdist_shift <- purrr::partial(stats::rnorm, mean = mean, sd = sd)
  }

  shift <- force(rdist_shift(n = 1))
  scale_factor <- force(rdist_scale(n = 1))
  patch_rescale(cols, shift = shift, scale_factor = scale_factor)
}


