#' \code{patch_identity} S3 class constructor
#'
#' @description
#' S3 class \code{patch_identity} which extends the \code{patch} and
#' \code{function} classes to represent the identity transformation of a
#' tabular dataset.
#'
#' @return A \code{patch_identity} object.
#'
#' @export
#'
#' @examples
#' p <- patch_identity()
#'
patch_identity <- function() {

  # Construct the patch object as a closure.
  obj <- function(df) { df }
  # Note: we prefer the above to the following alternative because it
  # encapsulates the enclosing environment (as required by patch functions).
  # obj <- identity

  class(obj) <- c("patch_identity", "patch", "function")
  obj
}
