#' Test for valid column identifiers
#'
#' Returns \code{TRUE} if \code{cols} is a non-empty vector of unique, non-NA
#' values each of which is either:
#' \itemize{
#'  \item a positive integer, or
#'  \item a non-empty character string
#' }
#'
#' @param cols
#' Any object.
#'
#' @return
#' \code{TRUE} if the \code{cols} object is a valid vector of column
#' identifiers and \code{FALSE} otherwise.
#'
#' @export
#'
#' @examples
#' is_valid_columns(c(2L, 5L))
#' is_valid_columns(c("mpg", "gear"))
#'
#' # Elements must be either non-empty character strings or positive integers.
#' is_valid_columns(c(2, 5))
#' is_valid_columns(c(0L, 5L))
#' is_valid_columns(c("mpg", ""))
#'
is_valid_columns <- function(cols) {

  if (length(cols) == 0)
    return(FALSE)
  if (any(duplicated(cols)) || any(is.na(cols)))
    return(FALSE)

  if (is.character(cols))
    return(all(nchar(cols) > 0))

  if (is.integer(cols))
    return(all(cols > 0))

  FALSE
}

