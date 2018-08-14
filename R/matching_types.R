#' Identify matching patch types
#'
#' Returns the vector of elementary patch types found in the given \code{patch}
#' which have a parameter with the given name.
#'
#' @param patch
#' A patch.
#' @param param_name
#' A scalar parameter name.
#' @param short
#' A logical flag passed to the \code{patch_type} function.
#'
#' @return The matching patch types as a character vector.
#'
#' @export
#'
#' @examples
#' patch <- compose_patch(patch_permute(10:1),
#'                        patch_rescale(1L, shift = 0, scale_factor = 2),
#'                        patch_delete(1L))
#' matching_types(patch, param_name = "shift")
#'
matching_types <- function(patch, param_name, short = TRUE) {

  stopifnot(length(param_name) == 1)
  types <- patch_type(patch, short = short, unique = TRUE)
  decomposed <- decompose_patch(patch)

  is_matching <- purrr::map_lgl(types, .f = function(type) {
    is_type_match <- purrr::map_lgl(decomposed, .f = function(p) {
      identical(patch_type(p, short = short), type)
    })
    all(purrr::map_lgl(decomposed[is_type_match], .f = function(p) {
      param_name %in% names(get_patch_params(p))
    }))
  })

  if (!any(is_matching))
    return(character(0))
  types[is_matching]
}
