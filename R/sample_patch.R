#' Randomly sample a composed patch
#'
#' @param df
#' A data frame
#' @param ...
#' n patch sampler functions to apply in order from right to left.
#' @param column_param
#' The name of the column index parameter in column-wise patch types, used to
#' ensure column-wise uniqueness of the sampled patch. Defaults to \code{cols}.
#' @param exclude_cols_arg
#' The name of the argument, in the given patch samplers for column-wise patch
#' types, used to exclude particular column indices from selection when sampling.
#' Used to ensure column-wise uniqueness of the sampled patch. Defaults to
#' \code{exclude_cols}.
#'
#' @export
#'
#' @examples
#' p <- sample_patch(mtcars, sample_patch_delete, sample_patch_permute)
#' p
#' head(p(mtcars))
#'
sample_patch <- function(df, ..., column_param = "cols",
                         exclude_cols_arg = "exclude_cols") {

  # Keep track of the effect on the data of patch samplers already
  # applied (e.g. deleted/inserted columns must be recognised by subsequent
  # elements in a composition of patches).
  running_patch <- patch_identity()

  # Keep track of the columns to which column-wise patches have been applied in
  # order to ensure that both:
  # 1. the sample patch is column-wise unique, and
  # 2. corrupted columns are not subsequently deleted.
  running_columnwise <- list()

  patch_list <- purrr::map(list(...), .f = function(patch_sampler) {

    # Take into account the running column index list when sampling.
    args <- list(running_patch(df))
    names(args) <- "df"

    delete_type <- patch_type(patch_delete(1L), short = TRUE)

    # Call the patch_sampler to determine the patch type. Note that this
    # affects the random number generator state, but does not impact
    # reproducibility.
    if (length(running_columnwise) > 0) {
      p_ <- patch_sampler(running_patch(df))
      if (patch_type(p_, short = TRUE) %in% names(running_columnwise)) {
        args <- c(args, running_columnwise[patch_type(p_, short = TRUE)])
        names(args)[names(args) == patch_type(p_, short = TRUE)] <- exclude_cols_arg
      }
    }

    p <- do.call(patch_sampler, args = args)

    # If the patch p operates column-wise (i.e. contains the given column
    # parameter and is *not* a delete patch), update the running column index list.
    type <- patch_type(p, short = TRUE)
    if (column_param %in% names(get_patch_params(p)) &&
        !identical(type, delete_type)) {

      cols <- get_patch_params(p)[[column_param]]
      if (type %in% names(running_columnwise))
        cols <- c(running_columnwise[[type]], cols)
      running_columnwise[[type]] <<- cols

      # Add all column-wise corrputed column indices to the running column index
      # list for type delete, so that corrupted columns are not subsequently deleted.
      cols <- get_patch_params(p)[[column_param]]
      if (delete_type %in% names(running_columnwise))
        cols <- unique(c(running_columnwise[[delete_type]], cols))
      running_columnwise[[delete_type]] <<- cols
    }

    # If the patch p affects the column positions (i.e. is an insert, delete
    # or permute patch), modify the running column index list appropriately.
    if (identical(type, patch_type(patch_insert(0L, data = data.frame()), short = TRUE)))
      sink <- sapply(names(running_columnwise), FUN = function(x) {
        running_cols <- running_columnwise[[x]]
        insertion_point <- get_patch_params(p)[["insertion_point"]]
        running_columnwise[[x]] <<- c(running_cols[running_cols < insertion_point],
                                      (running_cols[running_cols >= insertion_point] + 1L))
      })
    if (identical(type, patch_type(patch_delete(1L), short = TRUE)))
      sink <- sapply(names(running_columnwise), FUN = function(x) {
        running_cols <- running_columnwise[[x]]
        delete_col <- get_patch_params(p)[[column_param]]
        running_columnwise[[x]] <<- c(running_cols[running_cols < delete_col],
                                      (running_cols[running_cols > delete_col] - 1L))
      })
    if (identical(type, patch_type(patch_permute(1L), short = TRUE)))
      sink <- sapply(names(running_columnwise), FUN = function(x) {
        running_cols <- running_columnwise[[x]]
        perm <- get_patch_params(p)[["perm"]]
        running_columnwise[[x]] <<- perm[running_cols]
      })

    # Update the running patch composition.
    running_patch <<- compose_patch(p, running_patch)

    p
  })

  Reduce(compose_patch, rev(patch_list))
}
