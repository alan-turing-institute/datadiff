#' Extract the canonical elementary permutation from a patch
#'
#' The canonical elementary permutation is the unique permutation such that
#' applying the following procedure:
#'  1. form the patch \code{p} which is identical to the given \code{patch}
#'     except any permutation element is removed,
#'  2. apply the patch \code{p},
#'  3. apply the canonical elementary permutation,
#' produces the same column structure (in the resulting data frame) as that
#' produced by applying the given \code{patch}.
#'
#' Note that it is assumed that only the insert and delete patches modify
#' the number of columns in the data frame and only the permutation patch
#' modifies the column order (i.e. there are no other patch types that modify
#' the data frame column structure).
#'
#' @param patch
#' A patch.
#' @param perm_param
#' The name of the permutation parameter in patches of the relevant types.
#' @param param_only
#' A logical flag. If \code{TRUE}, the \code{perm} parameter value is returned,
#' rather than a permuation patch having that parameter. Defaults to \code{FALSE}.
#'
#' @return An elementary permutation patch.
#'
#' @export
#'
#' @examples
#' patch <- compose_patch(patch_delete(cols = 4L), patch_permute(perm = c(2:11, 1L)))
#' canonical_patch <- extract_canonical_permutation(patch)
#' colnames(patch(mtcars))
#' # To construct the equivalent patch we must delete the column that's
#' # in position 4 after the original permutation, i.e. column 5.
#' equiv_patch <- compose_patch(canonical_patch, patch_delete(cols = 5L))
#' colnames(equiv_patch(mtcars))
extract_canonical_permutation <- function(patch, perm_param = "perm",
                                          param_only = FALSE) {

  decomposed <- decompose_patch(patch)

  is_perm <- purrr::map_lgl(decomposed, .f = function(p) {
    perm_param %in% names(get_patch_params(p))
  })

  # If the given patch does not contain any permutation, return NA. Note that
  # returning the identity permutation is not an option since, absent any
  # elementary permutation in the patch, the number of columns is not known.
  if (!any(is_perm))
    return(NA)

  if (sum(is_perm) > 1)
    stop("Multiple permutations detected")

  is_delete <- patch_type(patch, short = TRUE, unique = FALSE) == "delete"
  is_insert <- patch_type(patch, short = TRUE, unique = FALSE) == "insert"

  # We only need to account for the inserts/deletes that occur *after* the
  # permutation, so we work with indices in the decomposition.
  i_perm <- which(is_perm)

  i_delete <- purrr::keep(which(is_delete), .p = function(j) { j > i_perm })
  i_insert <- purrr::keep(which(is_insert), .p = function(j) { j > i_perm })

  given_perm <- get_patch_params(decomposed[[i_perm]])[[perm_param]]
  canonical_perm <- given_perm

  # Map over the insert/delete patches (in order of application!)
  sink <- sapply(sort(union(i_delete, i_insert)), FUN = function(j) {
    # Get the insert/delete patch
    p <- decomposed[[j]]
    if (patch_type(p, short = TRUE) == "delete") {
      # Get the deleted column index
      column_index <- get_patch_params(p)[["cols"]]
      # Subtract 1 from all affected indices in the current perm parameter.
      is_affected <- canonical_perm > column_index
      canonical_perm[is_affected] <- canonical_perm[is_affected] - 1L
      # Delete the deleted column index.
      canonical_perm <<- canonical_perm[setdiff(1:length(canonical_perm), column_index)]
    }
    if (patch_type(p, short = TRUE) == "insert") {
      # Get the insertion point
      insertion_point <- get_patch_params(p)[["insertion_point"]]
      # Add 1 to all affected indices in the current perm parameter.
      is_affected <- canonical_perm > insertion_point
      canonical_perm[is_affected] <- canonical_perm[is_affected] + 1L
      # Insert the inserted column index.
      if (insertion_point == 0)
        pre <- integer(0)
      else
        pre <- canonical_perm[1:insertion_point]

      if (insertion_point == length(canonical_perm))
        post <- integer(0)
      else
        post <- canonical_perm[(insertion_point + 1):length(canonical_perm)]

      canonical_perm <<- c(pre, insertion_point + 1L, post)
    }
  })
  if (param_only)
    return(canonical_perm)
  patch_permute(perm = canonical_perm)
}
