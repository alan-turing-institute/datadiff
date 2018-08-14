#' Generate an insert patch
#'
#' Generates a \code{patch_insert} object which, when applied to \code{df1},
#' inserts a new column into that data frame (containing only \code{NA} values)
#' whose name matches that of \code{col2} in \code{df2}, prefixed by the string
#' given in the \code{prefix} argument.
#'
#' @param df1
#' A data frame.
#' @param df2
#' A data frame.
#' @param col1
#' A column identifier (integer or string column name) of length 1, specifying
#' the \code{insertion_point} of the new column in \code{df1}. By default this
#' takes the value of \eqn{col2 - 1}, so the column \emph{index} of the newly
#' inserted column in \code{df1} is the same as that of the \code{col2} in
#' \code{df2}.
#' @param col2
#' A column identifier (integer or string column name) of length 1.
#' @param prefix
#' A character string specifying the prefix to be added to the inserted column
#' name. The rest of the name is equal to that of \code{col2} in \code{df2}.
#' @param ...
#' Additional arguments are ignored.
#'
#' @return A \code{patch_insert} object.
#'
#' @seealso \code{\link{patch_insert}}
#'
#' @export
#'
#' @examples
#' gen_patch_insert(mtcars, mtcars, col2 = "wt")
#'
gen_patch_insert <- function(df1, df2, col1 = as.integer(col2 - 1),
                            col2, prefix = "INSERT.", ...) {

  # Do _not_ require that is_compatible_columns(col1, df1). Do this only when
  # the insert patch is applied, to allow for the possibility that other
  # insertions/changes to df1 are made between patch generation and application.
  if (is.character(col2))
    col1 <- which(colnames(df2) == col2)
  stopifnot(length(col1) == 1)
  stopifnot(is_compatible_columns(col2, df2) && length(col2) == 1)

  data <- data.frame(rep(NA, nrow(df1)))
  colnames(data) <- paste0(prefix, ifelse(is.character(col2), yes = col2,
                                          no = colnames(df2)[col2]))

  patch_insert(col1, data = data)
}
