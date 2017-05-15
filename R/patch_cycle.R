#' \code{patch_cycle} S3 class constructor
#'
#' @description
#' S3 class \code{patch_cycle} which extends the \code{patch} and
#' \code{function} classes to represent a transformation of a tabular dataset
#' by permutation of the column indices by a cycle.
#'
#' @param cols
#' A vector of column identifiers.
#'
#' @return A \code{patch_cycle} object.
#'
#' @export
#'
#' @seealso \code{\link{is_valid_columns}} \code{\link{is_compatible_columns}}
#' \code{\link{cycle}}
#'
#' @examples
#' colnames(mtcars)
#' p <- patch_cycle(c("mpg", "gear", "cyl"))
#' p <- patch_cycle(c(1L, 4L))
#'
#' # The following are equivalent:
#' colnames(apply_patch(mtcars, p))
#' colnames(p(mtcars))
#'
#' # Attempting to apply a patch to an incompatible data frame throws an error.
#' \dontrun{
#' p <- patch_cycle(c(1L, 3L, 22L))
#' p(mtcars)
#' }
patch_cycle <- function(cols) {

  stopifnot(is_valid_columns(cols) && length(cols) > 1)

  # Construct the patch object as a closure.
  obj <- function(df) {

    # Check for compatability between the data frame and the parameters.
    stopifnot(is_compatible_columns(cols, df))

    # Transform the data frame according to the parameters.
    if (is.character(cols))
      cols <- purrr::map_int(cols, function(x) { which(names(df) == x) })

    # Function to permute a vector according to a cycle.
    cycle <- function(v, cyc) {
      temp <- v
      rot <- c(cyc[-1], cyc[1])
      temp[cyc] <- temp[rot]
      temp
    }

    ret <- df[cycle(seq.int(1:ncol(df)), cyc = cols)]
    stopifnot(is.data.frame(ret))

    ret
  }

  class(obj) <- c("patch_cycle", "patch", "function")
  obj
}
