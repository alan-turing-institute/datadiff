#' \code{patch_shift} S3 class constructor.
#'
#' @description
#' S3 class \code{patch_shift} which extends the \code{patch} and
#' \code{function} classes to represent a transformation of a tabular dataset
#' by shifting (adding a constant) numerical data in one or more columns.
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
#' purrr::map_dbl(mtcars, mean)
#' p <- patch_shift(c(1L, 3L), 2)
#'
#' # The following are equivalent:
#' purrr::map_dbl(apply_patch(mtcars, p), mean)
#' purrr::map_dbl(p(mtcars), mean)
#'
#' # Attempting to apply a patch to an incompatible data frame throws an error.
#' \dontrun{
#' p <- patch_shift(c(1L, 22L), 2)
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
