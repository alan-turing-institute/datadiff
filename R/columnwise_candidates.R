#' Generate candidate patches by columnwise comparison
#'
#' @description
#' Helper function for \code{\link{ddiff}}. Generates a candidate patch for each
#' pair of columns (one each from \code{df1} & \code{df2}) and returns them in a
#' nested list.
#'
#' Possible candidates are:
#' \itemize{
#'   \item the identity patch
#'   \item a patch generated from one of the given \code{patch_generators}
#'   \item a break patch.
#' }
#' The "total cost" of each of these possible candidates is computed as the
#' sum of the (columwise) mismatch (after application of the patch) plus the
#' (scaled) penalty associated with the patch, except for the case of the break
#' patch for which the mismatch is always taken to be zero.
#'
#' The actual candidate is then the one with the minimum total cost.
#'
#' @param df1,df2
#' A pair of data frames.
#' @param mismatch
#' Mismatch method. The default is \code{\link{diffness}}.
#' @param patch_generators
#' A list of patch generator functions from which, for each pair of columns (one
#' each from \code{df1} & \code{df2}), candidate patches will be generated.
#' @param patch_penalties
#' A numeric vector of patch penalties corresponding to the \code{patch_generators}
#' list. The lengths of these two arguments must be equal.
#' @param break_penalty
#' The penalty associated with a break patch.
#' @param penalty_scaling
#' A function to be used to scale the penalty associated with each patch.
#' Defaults to \code{\link{ks_scaling}}.
#' @param mismatch_attr
#' The name of the attribute containing the calculated mismatch associated with
#' each candidate patch in the return value. Defaults to "mismatch".
#' @param penalty_attr
#' The name of the attribute containing the calculated penalty associated with
#' each candidate patch in the return value. Defaults to "penalty".
#' @param verbose
#' A logical flag.
#'
#' @return A nested list of patch objects with two numeric attributes:
#' \code{mismatch} and \code{penalty}.
#'
#' @export
#'
columnwise_candidates <- function(df1, df2,
                                mismatch = diffness,
                                patch_generators = list(gen_patch_transform),
                                patch_penalties = 0.2,
                                break_penalty = 0.95,
                                penalty_scaling = purrr::partial(ks_scaling, nx = nrow(df1), ny = nrow(df2)),
                                mismatch_attr = "mismatch",
                                penalty_attr = "penalty",
                                verbose = FALSE) {

  if (verbose)
    cat("Columnwise candidates' mismatch & penalty:\n")

  # Construct a nested list of candidate transformation (or break) patches and
  # simultaneously fill the costs & diffs matrices. Note that the costs + diffs
  # matrix cannot be processed until all elements have been calculated, after
  # which we must be able to recover the patches involved in those calculations.
  purrr::map(1:ncol(df1), .f = function(i) {
    purrr::map(1:ncol(df2), .f = function(j) {

      # Generate the column(pair)wise pre-candidate patch for to each generator,
      # then compute & attach the (numeric)  mismatch & penalty.
      patches <- purrr::map(1:length(patch_generators), .f = function(k) {
        gen <- patch_generators[[k]]
        p <- gen(df1, df2 = df2, mismatch = mismatch, col1 = i, col2 = j)
        if (!is.null(p)) {
          attr(p, mismatch_attr) <- mismatch(p(df1)[[i]], df2[[j]])
          attr(p, penalty_attr) <- penalty_scaling(patch_penalties[k])
        }
        p
      })

      # Add the identity patch and break patch to the patches list.
      # The identity patch has zero penalty & the break patch has zero mismatch.
      id_patch <- patch_identity()
      attr(id_patch, mismatch_attr) <- mismatch(df1[[i]], df2[[j]])
      attr(id_patch, penalty_attr) <- 0

      br_patch <- gen_patch_break(df1, df2 = df2, col1 = i, col2 = j)
      attr(br_patch, mismatch_attr) <- 0
      attr(br_patch, penalty_attr) <- penalty_scaling(break_penalty)

      patches <- c(list(id_patch), patches, list(br_patch))
      patches <- purrr::discard(patches, .p = is.null)

      # Identify which is the best candidate in the list.
      mismatches <- purrr::map_dbl(patches, .f = function(p) { attr(p, mismatch_attr) })
      penalties <- purrr::map_dbl(patches, .f = function(p) { attr(p, penalty_attr) })

      if (verbose) {
        cat(paste0("[", i, ", ", j, "]\n"))
        cat(round(mismatches, digits = 5), sep = "\t", "\n")
        cat(round(penalties, digits = 5), sep = "\t", "\n")
      }

      costs <- mismatches + penalties
      best <- min(which(costs == min(costs)))
      patches[[best]]
    })
  })
}
