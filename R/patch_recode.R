#' \code{patch_recode} S3 class constructor
#'
#' @description
#' S3 class \code{patch_recode} which extends the \code{patch} and
#' \code{function} classes to represent a transformation of a tabular dataset
#' by re-encoding categorical data in one or more columns.
#'
#' @param cols
#' A vector of column identifiers.
#' @param encoding
#' A non-empty vector whose elements have unique names. If \code{one_to_one} is
#' \code{TRUE} the elements must also be unique.
#' @param one_to_one
#' A logical flag specifying whether the \code{encoding} must be one-to-one.
#' Defaults to \code{TRUE}.
#'
#' @return A \code{patch_recode} object.
#'
#' @export
#'
#' @seealso \code{\link{is_valid_columns}} \code{\link{is_compatible_columns}}
#'
#' @examples
#' head(mtcars)
#'
#' # Columns 8 and 9 of mtcars contain binary data.
#' p <- patch_recode(c(8L, 9L), encoding = c("0" = FALSE, "1" = TRUE))
#'
#' # The following are equivalent:
#' head(apply_patch(mtcars, p))
#' head(p(mtcars))
#'
#' # Attempting to apply a patch to an incompatible data frame throws an error.
#' \dontrun{
#' # Column 1 does not contain binary data.
#' p <- patch_recode(1L, c("0" = FALSE, "1" = TRUE))
#' p(mtcars)
#' }
#'
patch_recode <- function(cols, encoding, one_to_one = TRUE) {

  # Check the given parameters are appropriate for the delete patch type.
  stopifnot(is_valid_columns(cols))
  stopifnot(length(names(encoding)) == length(encoding))
  stopifnot(!any(duplicated(names(encoding))))
  if (one_to_one)
    stopifnot(!any(duplicated(encoding)))

  # Construct the patch object as a closure.
  obj <- function(df) {

    # Check for compatability between the data frame and the parameters.
    stopifnot(is_compatible_columns(cols, df))

    # Identify any relevant factor columns.
    is_fac <- purrr::map_lgl(df[cols], is.factor)

    # Get all codes (note that union discards any duplicated values).
    codes <- purrr::discard(union(unlist(df[cols][!is_fac]),
                   unlist(purrr::map(df[cols][is_fac], levels))), is.na)

    if (!all(codes %in% names(encoding)))
      stop(paste("Invalid encoding omits codes:",
                 paste(setdiff(codes, names(encoding)), collapse = ", ")))

    # Transform the data frame according to the parameters.
    df[cols] <- purrr::map(cols, .f = function(i) {
      encoding[as.character(df[[i]])]
    })

    # Preserve any factors.
    if (any(is_fac))
      df[cols][is_fac] <- purrr::map(df[cols][is_fac], as.factor)
    df
  }

  class(obj) <- c("patch_recode", "patch", "function")
  obj
}
