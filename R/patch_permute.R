#' \code{patch_permute} S3 class constructor
#'
#' @description
#' S3 class \code{patch_permute} which extends the \code{patch} and
#' \code{function} classes to represent a transformation of a tabular dataset
#' by an arbitrary permutation of the column indices.
#'
#' @param perm A vector of integers, being a permutation of 1..n, where n is the
#'     number of columns in the dataset
#' @return A \code{patch_permute} object. When applied to a data set with columns
#'     \eqn{a_1, a_2, ..., a_n}, the result is a dataset with columns
#'     \eqn{a_{\mbox{perm}[1]}, a_{\mbox{perm}[2]}, ..., a_{\mbox{perm}[n]}}
#'
#' @export
#'
#' @seealso \code{\link{is_valid_columns}} \code{\link{is_compatible_columns}}
#'
#' @examples
#' head(mtcars)
#' p <- patch_permute(c(2L, 3L, 1L, 4:ncol(mtcars)))
#'
#' head(apply_patch(mtcars, p))
#'
#' # Attempting to apply a patch to an incompatible data frame throws an error.
#' \dontrun{
#' p <- patch_permute(1:(ncol(mtcars) + 1))
#' p(mtcars)
#' }
patch_permute <- function(perm) {

  stopifnot(is_valid_columns(perm) && length(perm) >= 1)
  stopifnot(setequal(perm, 1:max(perm)))

  # Construct the patch object as a closure.
  obj <- function(df) {

    # Check for compatability between the data frame and the parameters.
    stopifnot(is_compatible_columns(perm, df))
    stopifnot(length(perm) == ncol(df))

    # Transform the data frame according to the parameters.
    ret <- df[, perm]
    if (any(duplicated(colnames(df))))
      colnames(ret) <- purrr::map_chr(1:ncol(df), .f = function(i) {
        colnames(df)[perm][i]
      })

    stopifnot(is.data.frame(ret))

    ret
  }

  class(obj) <- c("patch_permute", "patch", "function")
  obj
}

#' Randomly sample from a distribution of permute patches
#'
#' @param df
#' A data frame
#' @param n
#' (Optional) An integer specifying the maximum number of columns to be moved
#' by sample permutations. Defaults to \code{NULL}, which means that all columns
#' will potentially be moved.
#' @param is_fixed
#' A logical vector of length equal to the number of columns in \code{df}. By
#' default, all elements are \code{FALSE} indicating that any column may
#' potentially be moved by sample permutations. The \code{is_fixed} argument
#' is ignored if \code{n} is not \code{NULL}.
#' @param exclude_identity
#' A logical flag specifying whether to exclude the identity permutation from
#' the set of possible sample permutations. Defaults to \code{TRUE}.
#' @param seed
#' A random seed.
#' @param ...
#' Any additional arguments are ignored.
#'
#' @export
#'
#' @examples
#' sample_patch_permute(mtcars)
#'
#' # Pick a transposition:
#' sample_patch_permute(mtcars, n = 2L)
#'
#' # Fix particular columns:
#' sample_patch_permute(mtcars, is_fixed = c(rep(TRUE, 6), rep(FALSE, 5)))
#'
sample_patch_permute <- function(df, n = NULL, is_fixed = rep(FALSE, ncol(df)),
                              exclude_identity = TRUE, seed, ...) {

  if (!missing(seed))
    set.seed(seed)

  ncols <- ncol(df)
  stopifnot(ncols > 1)

  if (!is.null(n)) {
    stopifnot(is.integer(n))
    stopifnot(n > 1 && n <= ncols)
    is_fixed <- rep(TRUE, ncols)
    is_fixed[sample.int(ncols, size = n, replace = FALSE)] <- FALSE
    return(sample_patch_permute(df, n = NULL, is_fixed = is_fixed,
                             exclude_identity = exclude_identity, seed = seed))
  }

  stopifnot(is.logical(is_fixed) && length(is_fixed) == ncols)

  if (sum(!is_fixed) < 2) {
    if (exclude_identity)
      stop("All columns fixed implies identity permutation.")
    else
      return(patch_permute(1:ncols))
  }

  moveable <- which(!is_fixed)
  moved <- sample(moveable, size = length(moveable), replace = FALSE)

  perm <- 1:ncols
  perm[!is_fixed] <- moved

  if (exclude_identity & identical(perm, 1:ncols))
    return(sample_patch_permute(df, is_fixed = is_fixed, exclude_identity = TRUE))

  patch_permute(perm)
}
