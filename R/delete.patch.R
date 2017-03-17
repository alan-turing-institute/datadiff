#' Test for compatibility between a patch and a data frame.
#'
#' @description
#' Implements the generic function \code{is_compatible} for a \code{patch} of type \code{delete.patch}. Returns \code{TRUE} if the given patch and data frame are compatible, in the sense that the patch (function) may be applied to the data frame without generating an error.
#'
#' The patch is deemed compatible if the maximum integer (in absolute value) in its \code{params} object does not exceed the number of columns in the data frame.
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
#' p <- patch(-2L)
#' is_compatible(p, mtcars)
#' p <- patch(-12L)
#' is_compatible(p, mtcars)
#'
#' @export
is_compatible.delete.patch <- function(obj, df, ...) {

  # Check that the (negative) column indices are not out-of-bounds.
  max(abs(patch_params(obj))) <= ncol(df)
}

#' Generate the patch return value.
#'
#' @description
#' Implements the generic function \code{return_value} for a \code{patch} of type \code{delete.patch}. Returns the data frame resulting from the application of the \code{patch} to the given data frame.
#'
#' In the case of a \emph{delete.patch} this is the given data frame minus those columns whose indices are specified in the patch \code{params} object.
#'
#' @param obj
#' A \code{patch} object.
#' @param df
#' A data frame.
#' @param ...
#' Any additional arguments are ignored.
#'
#' @examples
#' colnames(mtcars)
#'
#' p <- patch(-2L)
#' colnames(return_value(p, mtcars))
#'
#' @export
return_value.delete.patch <- function(obj, df, ...) {
  df[, patch_params(obj), drop = FALSE]
}
