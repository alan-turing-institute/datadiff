#' Test for columnwise uniqueness
#'
#' A composed patch is \emph{columnwise unique} for a particular patch type
#' if its decomposition into elementary patches contains at most one patch of
#' the given type applied to any particular column. An error is thrown if the
#' given \code{patch} does not contain any elementary component of the given
#' \code{type}.
#'
#' The \code{\link{terminal_column_position}} function is used to uniquely
#' identify each column because column indices are subject to modification
#' in the presence of insert, delete or permutation patches.
#'
#' @param patch
#' A patch object.
#' @param type
#' A patch type or vector of patch types. If \code{NULL} (the default), all
#' relevant patch types are tested.
#' @param short
#' A logical flag passed to the \code{\link{patch_type}} function. If
#' \code{TRUE} (the default) the \code{type} argument must specify the patch
#' type in short form.
#' @param column_param
#' The name of the column parameter in patches of the specified type.
#'
#' @return A logical vector of length equal to the \code{type} argument (or, if
#' \code{type} is \code{NULL}, the number of relevant elementary types in the
#' given \code{patch}). Each element is \code{TRUE} if the given patch is
#' columnwise unique for the corresponding type; otherwise \code{FALSE}. If the
#' \code{type} argument has length greater than one, the elements of the return
#' value are named by the corresponding type.
#'
#' @seealso \code{\link{terminal_column_position}}
#'
#' @export
#'
#' @examples
#' patch <- compose_patch(patch_rescale(3L, shift = 0, scale_factor = 2),
#'                        patch_delete(3L),
#'                        patch_rescale(4L, shift = 1, scale_factor = 10))
#' # Two rescale patches are applied to the same column:
#' is_columnwise_unique(patch)
#'
#' patch <- compose_patch(patch_recode(3L, encoding = c("N" = 0, "Y" = 1)),
#'                        patch_delete(3L),
#'                        patch_rescale(4L, shift = 1, scale_factor = 10))
#' # Two patches of different types are applied to the same column:
#' is_columnwise_unique(patch)
#'
is_columnwise_unique <- function(patch, type = NULL, short = TRUE,
                                 column_param = "cols") {

  decomposed <- decompose_patch(patch)

  # If type is NULL (the default) then test for all relevant types (i.e. all
  # patch types which have a parameter matching that given as the column_param
  # argument, excepting the delete patch type (to which the notion of column-wise
  # uniqueness does not apply).
  if (is.null(type)) {
    has_column_param <- purrr::map_lgl(decomposed, .f = function(p) {
      column_param %in% names(get_patch_params(p))
    })
    type <- unique(setdiff(purrr::map_chr(decomposed[has_column_param],
                                          .f = patch_type, short = TRUE),
                           y = patch_type(patch_delete(1L), short = TRUE)))
  }

  if (length(type) > 1)
    return(vapply(type, FUN.VALUE = logical(1), FUN = function(t) {
      is_columnwise_unique(patch, type = t, short = short, column_param = column_param)
    }))

  # Identify the patches in the decomposition of the given type.
  matching_type <- purrr::map_lgl(decomposed, .f = function(p) {
    identical(patch_type(p, short = short), type)
  })

  if (!any(matching_type))
    stop(paste("The given patch does not contain any components of type", type))

  # Return TRUE if the type is patch_insert (which is always columnwise unique).
  if (identical(type, patch_type(patch_insert(0L, data = data.frame(NA)),
                                 short = short)))
    return(TRUE)

  # Get all of the relevant column positions (*not* indices!).
  col_positions <- purrr::map(which(matching_type), .f = function(k) {
    patch <- decomposed[[k]]
    if (!(column_param %in% names(get_patch_params(patch))))
      stop(paste("Column index parameter not found in patch of type", type))
    column_indices <- get_patch_params(patch)[[column_param]]
    if (!is.integer(column_indices))
      stop("Non-integer column indices detected.")
    purrr::map_int(column_indices, .f = terminal_column_position,
                   patch = Reduce(compose_patch, rev(decomposed[1:k])),
                   initial = TRUE)
  })

  !any(duplicated(unlist(col_positions)))
}
