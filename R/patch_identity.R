#' \code{patch_identity} S3 class constructor.
#'
#' @description
#' S3 class \code{patch_cycle} which extends the \code{patch} and
#' \code{function} classes to represent the identity transformation of a tabular dataset
#'
#' @return A \code{patch_identity} object.
#'
#' @import purrr
#' @export
#'
#' @examples
#' id_p <- patch_identity()
#'
patch_identity <- function() {

  # Construct the patch object as a closure.
  obj <- identity

  class(obj) <- c("patch_identity", "patch", "function")
  obj
}
