#' \code{patch_insert} S3 class constructor
#'
#' @description
#' S3 class \code{patch_insert} which extends the \code{patch} and
#' \code{function} classes to represent a transformation of a tabular dataset
#' by inserting one or more new columns.
#'
#' When applied to a data frame, a \code{patch_insert} calls the
#' \code{dplyr::bind_cols} function to produce its result. As such, it is that
#' function which determines how column name conflicts are handled. As of
#' \code{dplyr} v0.7, this is done with a call to \code{tibble::repair_names}.
#'
#' @param insertion_point
#' A scalar column identifier. The new columns will be inserted immediately
#' after this column.
#' @param data
#' A data frame containing the new column data.
#'
#' @return A \code{patch_insert} object.
#'
#' @export
#'
#' @seealso \code{\link{is_valid_columns}} \code{\link{is_compatible_columns}}
#'
#' @examples
#' colnames(mtcars)
#' p <- patch_insert("gear", mtcars[2:4])
#' p <- patch_insert(0L, mtcars[2:4])
#'
#' # The following are equivalent:
#' colnames(apply_patch(mtcars, p))
#' colnames(p(mtcars))
#'
#' # Attempting to apply a patch to an incompatible data frame throws an error.
#' \dontrun{
#' p <- patch_insert(22L, mtcars[2:4])
#' p(mtcars)
#' }
patch_insert <- function(insertion_point, data) {

  # Check the given parameters are appropriate for the insert patch type.
  stopifnot(length(insertion_point) == 1)
  if (insertion_point != 0)
    stopifnot(is_valid_columns(insertion_point))

  stopifnot(is.data.frame(data))

  # Construct the patch object as a closure.
  obj <- function(df) {

    # Check for compatability between the data frame and the parameters.
    if (insertion_point != 0)
      stopifnot(is_compatible_columns(insertion_point, df))
    stopifnot(nrow(data) == nrow(df))

    if (is.character(insertion_point))
      insertion_point <- match(insertion_point, names(df))

    # Transform the data frame according to the parameters.
    post <- seq.int(insertion_point + 1, length(df))
    if (insertion_point == length(df))
      post <- 0
    ret <- dplyr::bind_cols(df[0:insertion_point], data, df[post])

    stopifnot(is.data.frame(ret))
    ret
  }

  class(obj) <- c("patch_insert", "patch", "function")
  obj
}
