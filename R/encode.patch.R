#' Test for compatibility between patch and data frame.
#'
#' @description
#' Implements the generic function \code{is_compatible} for a \code{patch} of type \code{encode.patch}. Returns \code{TRUE} if the given patch and data frame are compatible, in the sense that the patch (function) may be applied to the data frame without generating an error.
#'
#' The \code{patch} is deemed compatible if both:
#' \itemize{
#'  \item the maximum integer in the \code{datadiff:::COLUMNS} element of its \code{params} object does not exceed the number of columns in the data frame, and
#'  \item after coersion to type \code{character}, all of the values (or at least one of them, if \code{strict = FALSE}) in each of the relevant columns of the data frame are contained in the set of element names in the \code{datadiff:::ENCODING} element of the \code{params} object.
#' }
#'
#' @param obj
#' A \code{patch} object.
#' @param df
#' A data frame.
#' @param strict
#' A logical flag used to specify whether the encoding in the patch \code{params} object must include all of the values in the relevant columns (\code{strict = TRUE}), or merely at least one of those values (\code{strict = FALSE}). The default value is \code{TRUE}
#' @param ...
#' Any additional arguments are ignored.
#'
#' @examples
#' # Columns 8 and 9 of mtcars contain binary data.
#' p <- patch(encode_patch_params(c(8L, 9L), c("0" = FALSE, "1" = TRUE)))
#' is_compatible(p, mtcars)
#' # Other columns do not.
#' p <- patch(encode_patch_params(c(1L, 3L, 4L), c("0" = FALSE, "1" = TRUE)))
#' is_compatible(p, mtcars)
#'
#' # Encodings must be provided for all elements, unless strict = FALSE.
#' p <- patch(encode_patch_params(c(8L, 9L), c("0" = FALSE)))
#' is_compatible(p, mtcars)
#' is_compatible(p, mtcars, strict = FALSE)
#'
#' @export
is_compatible.encode.patch <- function(obj, df, strict = TRUE, ...) {

  params <- patch_params(obj)
  column_indices <- params[[datadiff:::COLUMNS]]
  encode_factor <- params[[datadiff:::ENCODING]]

  # Check that the column indices are not out-of-bounds.
  if (max(column_indices) > ncol(df))
    return(FALSE)

  # Check that the encoding is effectual. If strict is TRUE (respectively FALSE) then all (respectively at least one) of the values in the relevant columns must exist in the element names of the encode_factor.
  strictness <- ifelse(strict, yes = all, no = any)
  all(purrr::map_lgl(column_indices, .f = function(i) {
    strictness(as.character(df[, i]) %in% names(encode_factor))
  }))
}

#' Generate the patch return value.
#'
#' @description
#' Implements the generic function \code{return_value} for a \code{patch} of type \code{encode.patch}. Returns the data frame resulting from the application of the \code{patch} to the given data frame.
#'
#' In the case of an \emph{encode.patch} this is the given data frame after a re-encoding of the categorical data contained in those columns specified in the \code{params}, according to the specified encoding.
#'
#' Columns in the given data frame of type \code{factor} will also have type \code{factor} in the return value.
#'
#' @param obj
#' A \code{patch} object.
#' @param df
#' A data frame.
#' @param strict
#' A logical flag explained in the \code{\link{is_compatible.encode.patch}} method. If \code{strict} is \code{FALSE} and \code{NA} value(s) are introduced as a consequence, then a warning is generated.
#' @param convert_type
#' A logical flag. If \code{TRUE} (the default) then the \code{type.convert} function is used to convert the type of the encoding factor elements. If \code{FALSE} all re-encoded columns will have type \code{character}.
#' @param ...
#' Any additional arguments are ignored.
#'
#' @return A patched data frame.
#'
#' @examples
#' purrr::map_chr(mtcars, class)
#' p <- patch(encode_patch_params(c(8L, 9L), c("0" = FALSE, "1" = TRUE)))
#' purrr::map_chr(return_value(p, mtcars), class)
#'
#' @seealso \code{\link{type.convert}}
#' @import utils
#' @export
return_value.encode.patch <- function(obj, df, strict = TRUE, convert_type = TRUE, ...) {

  params <- patch_params(obj)
  column_indices <- params[[datadiff:::COLUMNS]]
  encode_factor <- params[[datadiff:::ENCODING]]

  # Apply the encoding in the factor, using type.convert to recover the type (with the as.is argument set so that, in the case of character encodings, each new column is a factor if and only if the original column was).
  df[, column_indices] <- purrr::map(column_indices, .f = function(i) {
    fac <- encode_factor[as.character(df[, i])]
    v <- levels(fac)[fac]
    if (convert_type)
      v <- utils::type.convert(v, as.is = TRUE)
    if (is.factor(df[, i]))
      v <- as.factor(v)
    if (!strict && any(is.na(v))) {
      if (!all(is.na(df[is.na(v), i])))
        warning("NAs introduced due to non-strict encoding.")
    }
    v
  })
  df
}

#' Construct parameters for the \code{encode.patch} type
#'
#' Converts a vector of integer column indices and an re-encoding of categorical data (in the form of a factor or a vector) into a \code{params} object suitable for constructing a patch of type \code{encode.patch}.
#'
#' @param column_indices
#' An integer vector of column indices
#' @param encode_factor
#' A factor (or vector) with named elements specifying a re-encoding of categorical data.
#'
#' @seealso \code{\link{patch}}
#'
#' @export
encode_patch_params <- function(column_indices = integer(), encode_factor = factor()) {

  encode_factor <- as.factor(encode_factor)
  if (!datadiff:::PREDICATE_COLUMNS(column_indices))
    stop("Invalid column_indices.")
  if (!datadiff:::PREDICATE_ENCODING(encode_factor))
    stop("Invalid encode_factor.")

  ret <- list(column_indices, encode_factor)
  names(ret) <- c(datadiff:::COLUMNS, datadiff:::ENCODING)
  ret
}
