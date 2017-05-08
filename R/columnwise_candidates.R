#' Generate candidate patches by columnwise comparison
#'
#' @description
#' TODO.
#'
#' @param df1,df2
#' A pair of data frames.
#' @param mismatch
#' Mismatch method. The default is (unscaled) \code{\link{diffness}}.
#' @param patch_generators
#' A list of patch generator functions.
#' @param patch_penalties
#' A vector of patch penalties corresponding to the \code{patch_generators}
#' list. The lengths of these two parameters must be equal.
#' @param verbose
#' A logical flag.
#'
#' @return A nested list of patch objects with two numeric attributes:
#' \code{mismatch} and \code{penalty}.
#'
#' @export
#'
# patch_penalties is a numeric vector of length equal to length(patch_generators).
columnwise_candidates <- function(df1, df2,
                                mismatch = purrr::partial(diffness, scale = FALSE),
                                patch_generators = list(gen_patch_transform),
                                patch_penalties = 0.2,
                                break_penalty = 0.95,
                                scale_penalty = scale_penalty,
                                verbose = FALSE) {

  if (verbose)
    cat("Columnwise candidates' mismatch & penalty:\n")

  # Construct a nested list of candidate transformation (or break) patches and
  # simultaneously fill the costs & diffs matrices. Note that the costs + diffs
  # matrix cannot be processed until all elements have been calculated, after
  # which we must be able to recover the patches involved in those calculations.
  purrr::map(1:ncol(df1), .f = function(i) {
    purrr::map(1:ncol(df2), .f = function(j) {

      generators <- c(
        gen_patch_identity,
        patch_generators,
        gen_patch_break
      )

      # Generate the column(pair)wise pre-candidate patch for to each
      # generator; then compute & attach the (numeric)  mismatch.
      patches <- purrr::map(generators, .f = function(gen) {
        p <- gen(df1, df2 = df2, mismatch = mismatch, col1 = i, col2 = j)
        if (!is.null(p))
          attr(p, "mismatch") <- attr(p, "mismatch")(p(df1)[[i]], df2[[j]])
        p
      })
      # Attach the (numeric) penalty to each patch.
      attr(patches[[1]], "penalty") <- 0
      attr(patches[[length(patches)]], "penalty") <-
        scale_penalty(break_penalty, nx = nrow(df1), ny = nrow(df2))
      sink <- sapply(1:length(patch_generators), FUN = function(k) {
        if (!is.null(patches[[k + 1]]))
          attr(patches[[k + 1]], "penalty") <<-
            scale_penalty(patch_penalties[k], nx = nrow(df1), ny = nrow(df2))
      })

      patches <- purrr::discard(patches, .p = is.null)

      # Identify which is the best candidate in the list.
      mismatches <- purrr::map_dbl(patches, .f = function(p) { attr(p, "mismatch") })
      penalties <- purrr::map_dbl(patches, .f = function(p) { attr(p, "penalty") })

      if (verbose) {
        cat(paste0("[", i, ", ", j, "]\n"))
        cat(mismatches, sep = "\t", "\n")
        cat(round(penalties, digits = 5), sep = "\t", "\n")
      }

      costs <- mismatches + penalties
      best <- min(which(costs == min(costs)))
      patches[[best]]
    })
  })
}
