#' Test for compatibility between a patch and a data frame.
#'
#' @description
#' Implements the generic function \code{is_compatible} for a \code{patch} of type \code{permute.patch}. Returns \code{TRUE} if the given patch and data frame are compatible, in the sense that the patch (function) may be applied to the data frame without generating an error.
#'
#' The patch is deemed compatible if the maximum integer in its \code{params} object does not exceed the number of columns in the data frame.
#'
#' @param obj
#' A \code{patch} object.
#' @param df
#' A data frame.
#' @param ...
#' Any additional arguments are ignored.
#'
#' @examples
#' ncol(mtcars)
#'
#' p <- patch(c(1L, 3L, 4L))
#' is_compatible(p, mtcars)
#' p <- patch(c(1L, 3L, 12L))
#' is_compatible(p, mtcars)
#'
#' @export
is_compatible.permute.patch <- function(obj, df, ...) {

  # Check that the column indices are not out-of-bounds.
  max(patch_params(obj)) <= ncol(df)
}

#' Generate the patch return value.
#'
#' @description
#' Implements the generic function \code{return_value} for a \code{patch} of type \code{permute.patch}. Returns the data frame resulting from the application of the \code{patch} to the given data frame.
#'
#' In the case of a \emph{permute.patch} this is the given data frame with its columns permuted according to the vector of indices specified in the patch \code{params} object, which is interpreted as a \link{cycle}.
#'
#' @seealso \code{\link{cycle}}
#'
#' @param obj
#' A \code{patch} object.
#' @param df
#' A data frame.
#' @param ...
#' Any additional arguments are ignored.
#'
#' @return A patched data frame.
#'
#' @examples
#' colnames(mtcars)
#'
#' p <- patch(c(1L, 3L, 4L))
#' colnames(return_value(p, mtcars))
#'
#' @export
return_value.permute.patch <- function(obj, df, ...) {
  df[, cycle(seq.int(1:ncol(df)), cyc = patch_params(obj))]
}
