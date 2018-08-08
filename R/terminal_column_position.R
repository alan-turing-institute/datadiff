#' Infer a terminal column position
#'
#' Returns the terminal column position corresponding to a given column index in
#' a patched data frame (i.e. before or after application of a patch). Takes into
#' account the three standard patch types whose application affects column positions:
#' \code{insert}, \code{delete} and \code{permute}.
#'
#' @param column_index
#' A scalar integer column index specifying the column of interest \emph{after}
#' application of the \code{patch}, if \code{initial} is \code{TRUE} or
#' \emph{before} application of the \code{patch} if \code{initial} is \code{FALSE}.
#' @param patch
#' A patch object in which all column identifiers are integer column indices (not
#' column names).
#' @param initial
#' A logical flag. If \code{TRUE} (the default), the initial column position is
#' returned (i.e. the position which is moved by the given \code{patch} to the
#' given \code{column_index}). Otherwise the final column position is
#' returned (i.e. the position to which the given \code{column_index} is moved
#' by the given \code{patch}).
#'
#' @return An integer column position.
#'
#' @seealso \code{decompose_patch}
#'
#' @export
#'
#' @examples
#' # A rescale patch does not affect column position:
#' terminal_column_position(4L, patch_rescale(2L, shift = 0, scale_factor = 2))
#' # Delete patch:
#' terminal_column_position(4L, patch_delete(1L))
#' # Insert patch:
#' terminal_column_position(4L, patch_insert(1L, data.frame(NA)))
#' # Permute patch:
#' terminal_column_position(4L, patch_permute(4:1))
#' # Composed patch:
#' terminal_column_position(4L, compose_patch(patch_permute(4:1), patch_delete(1L)))
#'
terminal_column_position <- function(column_index, patch, initial = TRUE) {

  stopifnot(length(column_index) == 1)
  stopifnot(all(column_index > 0))

  decomposed <- decompose_patch(patch)

  # If we are moving backwards, to the initial condition, consider the last
  # elementary patch to be applied. If we are moving forwards, consider the first.
  component_index <- ifelse(initial, yes = length(decomposed), no = 1L)
  component <- decomposed[[component_index]]

  new_index <- column_index

  # Case: component is an insert patch.
  if (identical(patch_type(component, short = TRUE),
                patch_type(patch_insert(0L, data = data.frame(NA)), short = TRUE))) {
    insertion_index <- get_patch_params(component)[["insertion_point"]]
    if (!is.integer(insertion_index))
      stop("Non-integer column index detected in insert patch.")
    if (initial && column_index %in% (insertion_index + 1))
      stop(paste("Index", column_index, "corresponds to an inserted column"))
    factor <- ifelse(initial, yes = -1, no = 1)
    shift <- as.integer(factor * sum(insertion_index < column_index))
    new_index <- sum(column_index, shift)
  }
  # Case: component is a delete patch.
  if (identical(patch_type(component, short = TRUE),
                patch_type(patch_delete(1L), short = TRUE))) {
    deletion_index <- get_patch_params(component)[["cols"]]
    if (!is.integer(deletion_index))
      stop("Non-integer column index detected in delete patch.")
    if (!initial && column_index %in% (deletion_index))
      stop(paste("Index", column_index, "corresponds to a deleted column"))
    factor <- ifelse(initial, yes = 1, no = -1)
    shift <- as.integer(factor * sum(deletion_index <= column_index))
    new_index <- sum(column_index, shift)
  }
  # Case: component is a permute patch.
  if (identical(patch_type(component, short = TRUE),
                patch_type(patch_permute(1L), short = TRUE))) {
    perm <- get_patch_params(component)[["perm"]]
    if (!is.integer(perm))
      stop("Non-integer column indices detected in permute patch.")
    if (!(column_index %in% perm))
      stop(paste("Column index", column_index, "incompatible with permutation:",
                 paste(perm, collapse = " ")))
    if (initial)
      new_index <- which(order(perm) == column_index)
    else
      new_index <- order(perm)[column_index]
  }

  # If the patch is not composed, return the new index.
  if (length(decomposed) == 1)
    return(new_index)

  # Otherwise, recursively reduce the number of elements in the decomposition.
  new_patch <- Reduce(compose_patch, rev(decomposed[-component_index]))
  terminal_column_position(new_index, patch = new_patch, initial = initial)
}
