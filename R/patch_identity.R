#' \code{patch_identity} S3 class constructor
#'
#' @description
#' S3 class \code{patch_identity} which extends the \code{patch} and
#' \code{function} classes to represent the identity transformation.
#'
#' @return A \code{patch_identity} object.
#'
#' @export
#'
#' @examples
#' patch_identity()
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

#' 'Sample' an identity patch
#'
#' A trivial helper function (since the identity patch is unique). It is
#' identical to the \code{patch_identity} constructor except it accepts (and
#' ignores) any arguments.
#'
#' @param ...
#' Arguments are ignored.
#'
#' @export
sample_patch_identity <- function(...) {
  patch_identity()
}
