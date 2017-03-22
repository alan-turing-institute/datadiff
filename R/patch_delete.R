#' \code{patch_delete} S3 class constructor
#'
#' @description
#' S3 class \code{patch_delete} which extends the \code{patch} and
#' \code{function} classes to represent a transformation of a tabular dataset
#' by deletion of one or more columns.
#'
#' @param cols
#' A vector of column identifiers.
#'
#' @return A \code{patch_delete} object.
#'
#' @export
#'
#' @seealso \code{\link{is_valid_columns}} \code{\link{is_compatible_columns}}
#'
#' @examples
#' colnames(mtcars)
#' p <- patch_delete(c(2L, 5L))
#' p <- patch_delete(c("mpg", "gear"))
#'
#' # The following are equivalent:
#' colnames(apply_patch(p, mtcars))
#' colnames(p(mtcars))
#'
#' # Attempting to apply a patch to an incompatible data frame throws an error.
#' \dontrun{
#' p <- patch_delete(22L)
#' p(mtcars)
#' }
patch_delete <- function(cols) {

  # Check the given parameters are appropriate for the delete patch type.
  stopifnot(is_valid_columns(cols))

  # Construct the patch object as a closure.
  obj <- function(df) {

    # Check for compatability between the data frame and the parameters.
    stopifnot(is_compatible_columns(cols, df))

    # Transform the data frame according to the parameters.
    if (is.integer(cols))
      i <- -cols
    else
      i <- setdiff(names(df), cols)
    ret <- df[, i, drop = FALSE]

    stopifnot(is.data.frame(ret))
    ret
  }

  class(obj) <- c("patch_delete", "patch", "function")
  obj
}
