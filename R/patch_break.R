#' \code{patch_break} S3 class constructor
#'
#' @description
#' S3 class \code{patch_break} which extends the \code{patch} and
#' \code{function} classes to represent a transformation of a tabular dataset
#' by an abrubt break in one or more columns.
#'
#' Break patches are unique in that the mismatch between a broken column and
#' any other column is always taken to be zero.
#'
#' @param cols
#' A vector of column identifiers.
#' @param data
#' A data frame containing the new column data. The number of columns in
#' \code{data} must be equal to the length of \code{cols}.
#'
#' @return A \code{patch_break} object.
#'
#' @export
#'
#' @seealso \code{\link{is_valid_columns}} \code{\link{is_compatible_columns}}
#'
patch_break <- function(cols, data) {

  stopifnot(ncol(data) == length(cols))

  # Define a list of break patches (one for each column) each of which is an
  # insert-then-delete composition. Note that delete-then-insert would be
  # problematic in the case of character column identifiers when the first
  # column is being broken (as we would have to insert at the zero'th position).
  patch_list <- purrr::map(1:length(cols), .f = function(i) {

    p_insert <- patch_insert(insertion_point = cols[i],
                             data = data[, i, drop = FALSE])
    p_delete <- patch_delete(cols = cols[i])

    compose_patch(p_delete, p_insert)
  })

  Reduce(compose_patch, rev(patch_list))
}
