#' \code{patch_perm} S3 class constructor
#'
#' @description
#' S3 class \code{patch_perm} which extends the \code{patch} and
#' \code{function} classes to represent a transformation of a tabular dataset
#' by an arbitrary permutation of the column indices.
#'
#' @param perm A vector of integers, being a permutation of 1..n, where n is the
#'     number of columns in the dataset
#' @return A \code{patch_perm} object. When applied to a data set with columns
#'     a_1, a_2, ..., a_n, the result is a dataset with columns a_perm[1],
#'     a_perm[2], ...
#'
#' @export
#'
#' @seealso \code{\link{is_valid_columns}} \code{\link{is_compatible_columns}}
#'
#' @examples
#' colnames(mtcars)
#' p <- patch_perm(c(2L, 3L, 1L))
#'
##' apply_patch(p, mtcars)
##'
#' # Attempting to apply a patch to an incompatible data frame throws an error.
#' \dontrun{
#' p <- patch_perm(c(1L, 3L, 22L))
#' p(mtcars)
#' }
patch_perm <- function(perm) {

  stopifnot(is_valid_columns(perm) && length(perm) >= 1)

  # Construct the patch object as a closure.
  obj <- function(df) {

    # Check for compatability between the data frame and the parameters.
    stopifnot(is_compatible_columns(perm, df))

    # Transform the data frame according to the parameters.
    ret <- df[, perm]
    stopifnot(is.data.frame(ret))

    ret
  }

  class(obj) <- c("patch_perm", "patch", "function")
  obj
}

