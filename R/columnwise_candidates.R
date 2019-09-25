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
#' sum of the residual (columwise) mismatch (after application of the patch)
#' plus the (scaled) penalty associated with the patch, except for the case of
#' the break patch for which the mismatch is always taken to be zero and the
#' penalty is not scaled.
#'
#' The actual candidate is then the one with the minimum total cost.
#'
#' @details
#' Each element in the \code{patch_generators} list must be a patch generator
#' function. That is, a function, with arguments \code{df1}, \code{df2},
#' \code{mismatch}, \code{col1} and \code{col2}, which returns a patch such that
#' the mismatch between column \code{col2} in \code{df2} and the patched column
#' \code{col1} in \code{df1} is minimised to the extent possible using the type
#' of patch in question.
#'
#' @param df1,df2
#' A pair of data frames.
#' @param mismatch
#' Mismatch method. The default is \code{\link{diffness}}.
#' @param patch_generators
#' A list of patch generator functions from which candidate patches will be
#' generated for every pair of columns (one each from \code{df1} & \code{df2}).
#' @param patch_penalties
#' A numeric vector of patch penalties corresponding to the \code{patch_generators}
#' list. The lengths of these two arguments must be equal.
#' @param break_penalty
#' The penalty associated with a break patch. No scaling is applied to the
#' \code{break_penalty}.
#' @param penalty_scaling
#' A function to be used to scale the penalty associated with each patch.
#' Defaults to \code{\link{ks_scaling}}.
#' @param mismatch_attr
#' The name of the attribute in the return value containing the calculated mismatch associated with
#' each candidate patch. Defaults to "mismatch".
#' @param penalty_attr
#' The name of the attributein the return value containing the calculated penalty associated with
#' each candidate patch. Defaults to "penalty".
#' @param scale_break_penalty
#' A logical flag to turn on/off scaling of the break penalty. Defaults to
#' \code{FALSE}.
#' @param verbose
#' A logical flag.
#'
#' @return A nested list of patch objects, each with two numeric attributes:
#' one for the residual columnwise mismatch and one for the patch penalty.
#'
#' @export
#'
columnwise_candidates <- function(df1, df2,
                                  mismatch,
                                  constraints,
                                  patch_generators,
                                  patch_penalties,
                                  break_penalty,
                                  penalty_scaling,
                                  mismatch_attr = "mismatch",
                                  penalty_attr = "penalty",
                                  scale_break_penalty = FALSE,
                                  verbose = FALSE) {

  if (verbose)
    cat("Columnwise candidates' mismatch & penalty:\n")

  # Construct a nested list of candidate patches with attributes for the
  # residual mismatch and the associated penalty.
  purrr::map(1:ncol(df1), .f = function(i) {
    # If there is notransform constraint for column df[i], we will skip all patches
    has_notransform <- 1 == length(purrr::keep(constraints, function(x) {
      is_constraint_notransform(x, names(df1)[i]) }))
    # If there is match constraint for this column with another, we return 'never' patch 9999
    has_match_col1 <- 1 == length(purrr::keep(constraints, function(x) {
      is_constraint_match_col1(x, names(df1)[i]) }))

    purrr::map(1:ncol(df2), .f = function(j) {
      # If there is match constraint for this column with another, we return 'never' patch with 9999
      has_match_col2 <- 1 == length(purrr::keep(constraints, function(x) {
        is_constraint_match_col2(x, names(df2)[j]) }))

      # If there is nomatch constraint, we return 'never' patch
      has_nomatch <- 1 == length(purrr::keep(constraints, function(x) {
        is_constraint_nomatch(x, names(df1)[i], names(df2)[j]) }))
      if (has_nomatch || (has_match_col1 && !has_match_col2) || (has_match_col2 && !has_match_col1)) {
        p <- patch_never()
        attr(p, mismatch_attr) <- 0
        attr(p, penalty_attr) <- 9999
        return(p)
      }

      # Generate the column(pair)wise pre-candidate patch for to each generator,
      # then compute & attach the (numeric)  mismatch & penalty.
      if (has_notransform) patches <- list()
      else patches <- purrr::map(1:length(patch_generators), .f = function(k) {
        gen <- patch_generators[[k]]

        p <- tryCatch(
          gen(df1, df2 = df2, mismatch = mismatch, col1 = i, col2 = j),
          error = function(e) { NULL })

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
      if (scale_break_penalty)
        break_penalty <- penalty_scaling(break_penalty)
      # Broken columns must not be permuted.
      attr(br_patch, penalty_attr) <- ifelse(i == j, yes = break_penalty, no = 1.0)

      patches <- c(list(id_patch), patches, list(br_patch))
      patches <- purrr::discard(patches, .p = is.null)

      # Identify which is the best candidate in the list.
      mismatches <- purrr::map_dbl(patches, attr, mismatch_attr)
      penalties <- purrr::map_dbl(patches, attr, penalty_attr)

      if (verbose) {
        cat(paste0("[", i, ", ", j, "]\n"))
        cat(round(mismatches, digits = 5), sep = "\t", "\n")
        cat(round(penalties, digits = 5), sep = "\t", "\n")
      }

      patches[[which.min(mismatches + penalties)]]
    })
  })
}
