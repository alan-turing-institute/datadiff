#' Extract the patch type from a columnwise patch generator function
#'
#' Deduces the patch type of an elementary patch generator function by applying
#' the generator to the given data (after splitting, using \code{split_data}).
#'
#' Vectorised over the \code{patch_generator} argument (which may be a list).
#'
#' @param df
#' A data frame.
#' @param patch_generator
#' A patch generator function or a list of such.
#' @param short
#' A logical flag passed to the \code{\link{patch_type}} function. Defaults to
#' \code{TRUE}.
#'
#' @return A character vector containing the patch type corresponding to the
#' \code{patch_generator} (whose order corresponds to that of the
#' \code{patch_generator} in case it is a list of generator functions). An
#' \code{NA} value indicates that the corresponding generator function is
#' incompatible with \emph{all} columns in the data, and therefore the patch
#' type could not be deduced.
#'
#' @export
#'
#' @examples
#' generator_type(mtcars, gen_patch_rescale)
#' generator_type(esoph, list(gen_patch_rescale, gen_patch_recode))
#'
generator_type <- function(df, patch_generator, short = TRUE) {

  if (!is.list(patch_generator))
    patch_generator <- list(patch_generator)

  # Construct a nested list containing all patch types by applying the given
  # patch_generators to each column of the split data.
  dfs <- split_data(df, split = 0.5)
  patches <- purrr::map(patch_generator, .f = function(gen) {
    purrr::discard(
      purrr::map(1:ncol(df), .f = function(col) {
        tryCatch(
          gen(df1 = dfs[[1]], df2 = dfs[[2]], col1 = col),
          error = function(e) { NULL })
      }),
      .p = is.null)
  })

  # Identify all relevant patch types (in order corresponding to the
  # patch_generators vector). The patch type in each sublist of 'patches' is
  # unique (because the same elementary patch generator was used). Assign NA
  # for any patch type that is incompatible with _all_ columns.
  ret <- purrr::map_chr(patches, .f = function(sublist) {
    type <- unique(purrr::map_chr(sublist, .f = patch_type, short = short))
    if (length(type) == 0)
      return(NA)
    type
  })

  # There is no identity patch generator so that type ought not to appear.
  if (patch_type(patch_identity(), short = short) %in% ret)
    stop("Failed to extract generator type. Only columnwise types are supported.")

  ret
}
