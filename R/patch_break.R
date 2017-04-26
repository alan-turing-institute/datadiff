#' \code{patch_break} S3 class constructor
#'
#' @description
#' S3 class \code{patch_break} which extends the \code{patch} and
#' \code{function} classes to represent a transformation of a tabular dataset
#' by an abrubt break in one or more columns.
#'
#' @param cols
#' A vector of column identifiers.
#'
#' @return A \code{patch_break} object.
#'
#' @export
#'
#' @seealso \code{\link{is_valid_columns}} \code{\link{is_compatible_columns}}
#'
patch_break <- function(cols) {

  # TODO: the current implementation, which is temporary and intended only for
  # testing, replaces all values in the specified columns with NAs.

  # Check the given parameters are appropriate for the delete patch type.
  stopifnot(is_valid_columns(cols))

  # Construct the patch object as a closure.
  obj <- function(df) {

    # Check for compatability between the data frame and the parameters.
    stopifnot(is_compatible_columns(cols, df))

    # Transform the data frame.
    df[cols] <- NA

    stopifnot(is.data.frame(df))
    df
  }

  class(obj) <- c("patch_break", "patch", "function")
  obj
}
