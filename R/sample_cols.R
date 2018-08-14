#' Choose column indices at random
#'
#' @param df
#' A data frame.
#' @param condition
#' A predicate to determine valid candidate columns. An error is thrown if this
#' predicate returns \code{FALSE} when called on every column in the given data
#' frame.
#' @param exclude_cols
#' An integer vector of column indices to be excluded (i.e. not chosen). An error
#' is thrown if all of the candidate columns, as identified by the
#' \code{condition} predicate, are excluded by this argument.
#' @param size
#' The number of samples (i.e. column indices) to pick.
#'
#' @return An integer vector of column indices
#'
#' @export
#'
#' @examples
#' # Choose any numeric column:
#' sample_cols(mtcars, condition = is.numeric)
#' # Choose a column whose values are binary:
#' sample_cols(mtcars, condition = function(x) { all(x %in% c(0, 1)) })
#'
sample_cols <- function(df, condition = is.numeric, exclude_cols = integer(0),
                        size = 1) {

  stopifnot(is.data.frame(df))
  stopifnot(all(exclude_cols %in% 1:ncol(df)))
  is_candidate_col <- purrr::map_lgl(df, condition)

  if (!any(is_candidate_col))
    stop("No columns satisfy the given condition.")

  candidate_cols <- which(is_candidate_col)

  # Exclude the columns in the exclude_cols argument.
  candidate_cols <- setdiff(candidate_cols, exclude_cols)
  if (length(candidate_cols) == 0)
    stop("All candidate columns are excluded.")

  # Use the sample function with care (i.e. avoid a scalar first argument).
  if (length(candidate_cols) == 1)
    return(candidate_cols)
  sample(candidate_cols, size = size, replace = FALSE)
}
