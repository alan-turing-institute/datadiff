#' Test for compatibility between patch and data frame.
#'
#' @description
#' Implements the generic function \code{is_compatible} for a \code{patch} of type \code{scale.patch}. Returns \code{TRUE} if the given patch and data frame are compatible, in the sense that the patch (function) may be applied to the data frame without generating an error.
#'
#' The \code{patch} is deemed compatible if both:
#' \itemize{
#'  \item the maximum integer in the \code{datadiff:::COLUMNS} element of its \code{params} object does not exceed the number of columns in the data frame, and
#'  \item all of the columns in the data frame whose indices are specified in the \code{datadiff:::COLUMNS} element contain numerical data.
#' }
#'
#' @param obj
#' A \code{patch} object.
#' @param df
#' A data frame.
#' @param ...
#' Any additional arguments are ignored.
#'
#' @examples
#' # Columns 1 and 3 in the ToothGrowth data frame are numeric.
#' p <- patch(scale_patch_params(c(1L, 3L), 2))
#' is_compatible(p, ToothGrowth)
#' # Column 2 is not.
#' p <- patch(scale_patch_params(c(1L, 2L), 2))
#' is_compatible(p, ToothGrowth)
#'
#' @import purrr
#' @export
is_compatible.scale.patch <- function(obj, df, ...) {

  params <- patch_params(obj)
  column_indices <- params[[datadiff:::COLUMNS]]

  if (max(column_indices) > ncol(df))
    return(FALSE)

  all(purrr::map_lgl(df[, column_indices], is.numeric))
}

#' Generate the patch return value.
#'
#' @description
#' Implements the generic function \code{return_value} for a \code{patch} of type \code{scale.patch}. Returns the data frame resulting from the application of the \code{patch} to the given data frame.
#'
#' In the case of a \emph{scale.patch} this is the given data frame after a rescaling of the numerical data contained in those columns specified in the \code{params}, by the specified scale factor.
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
#' purrr::map_dbl(ToothGrowth[c(1, 3)], sum)
#' p <- patch(scale_patch_params(c(1L, 3L), 2))
#' purrr::map_dbl(p(ToothGrowth)[c(1, 3)], sum)
#'
#' @seealso \code{\link{type.convert}}
#' @import utils
#' @export
return_value.scale.patch <- function(obj, df, ...) {

  params <- patch_params(obj)
  column_indices <- params[[datadiff:::COLUMNS]]
  scale_factor <- params[[datadiff:::SCALE_FACTOR]]

  df[, column_indices] <- purrr::map(column_indices, .f = function(i) {
    scale_factor * df[, i]
  })
  df
}

#' Construct parameters for the \code{scale.patch} type
#'
#' Converts a vector of integer column indices and a numerical scale factor into a \code{params} object suitable for constructing a patch of type \code{scale.patch}.
#'
#' @param column_indices
#' An integer vector of column indices
#' @param scale_factor
#' A numerical scalar specifying a scale factor.
#'
#' @seealso \code{\link{patch}}
#'
#' @export
scale_patch_params <- function(column_indices = integer(), scale_factor = numeric()) {

    if (!datadiff:::PREDICATE_COLUMNS(column_indices))
    stop("Invalid column_indices.")
  if (length(scale_factor) != 1 || !is.numeric(scale_factor))
    stop("Invalid scale_factor.")

  ret <- list(column_indices, scale_factor)
  names(ret) <- c(datadiff:::COLUMNS, datadiff:::SCALE_FACTOR)
  ret
}
