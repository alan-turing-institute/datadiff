#' Test for compatibility between a data frame and column identifiers
#'
#' Returns \code{TRUE} if the given column identifiers are compatible with the
#' given data frame.
#'
#' @param cols
#' A vector of column identifiers (character or integer).
#' @param df
#' A data frame.
#'
#' @return
#' \code{TRUE} if the given column identifiers are compatible with the given
#' data frame, otherwise \code{FALSE}.
#'
#' @export
#'
#' @examples
#' is_compatible_columns(c(2L, 5L), mtcars)
#' is_compatible_columns(c("mpg", "gear"), mtcars)
#'
#' is_compatible_columns(c(2L, 22L), mtcars)
#' is_compatible_columns(c("mpg", "xxx"), mtcars)
#'
is_compatible_columns <- function(cols, df) {

  if (!is_valid_columns(cols))
    return(FALSE)

  ifelse(is.character(cols), yes = all(cols %in% colnames(df)),
         no = max(cols) <= ncol(df))
}

