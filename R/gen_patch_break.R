#' Generate a break patch
#'
#' Generates a \code{patch_break} object.
#'
#' @param df1
#' A data frame.
#' @param df2
#' A data frame.
#' @param col1
#' A column identifier (integer or string column name) of length 1.
#' @param col2
#' A column identifier (integer or string column name) of length 1. By default
#' this takes the value of \code{col1}.
#' @param ...
#' Additional arguments are ignored.
#'
#' @return A \code{patch_break} object.
#'
#' @seealso \code{\link{patch_break}}
#'
#' @export
#'
#' @examples
#' gen_patch_break(mtcars, mtcars, col1 = "wt")
#'
gen_patch_break <- function(df1, df2, col1,
                            col2 = col1, ...) {

  stopifnot(is_compatible_columns(col1, df1) && length(col1) == 1)
  stopifnot(is_compatible_columns(col2, df2) && length(col2) == 1)

  v1 <- df1[[col1]]
  v2 <- df2[[col2]]

  # old: stopifnot(sum(!is.na(v1)) != 0 && sum(!is.na(v2)) != 0)

  # The new column consists entirely of NAs.
  data <- data.frame(rep(NA, times = nrow(df1)))
  names(data) <- ifelse(is.character(col2), yes = col2, no = colnames(df2)[col2])

  patch_break(col1, data = data)
}
