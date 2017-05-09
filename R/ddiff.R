#' Diff two data frames
#'
#' @description
#' Given two data frames and a collection of patch generator functions, with
#' associated (numeric) penalties, this function returns a composite patch
#' object representing the sequence of transformations by which the data frames
#' differ.
#'
#' @param df1,df2
#' A pair of data frames.
#' @param mismatch
#' Mismatch method. The default is (unscaled) \code{\link{diffness}}.
#' @param patch_generators
#' A list of patch generator functions from which, for each pair of columns (one
#' each from \code{df1} & \code{df2}), candidate patches will be generated.
#' @param patch_penalties
#' A numeric vector of patch penalties corresponding to the \code{patch_generators}
#' list. The lengths of these two arguments must be equal.
#' @param break_penalty
#' The penalty associated with a break patch.
#' @param perm_penalty
#' The penalty associated with a permutation patch.
#' @param penalty_scaling
#' A function to be used to scale the penalty associated with each patch.
#' Defaults to \code{\link{ks_scaling}}.
#' @param as.list
#' A logical flag. If \code{TRUE} the return value is a list of patches.
#' Otherwise a composite patch object is returned.
#' @param verbose
#' A logical flag.
#'
#' @return A composite patch object or, if \code{as.list} is \code{TRUE}, its
#' decomposition as a list of elementary patches.
#'
#' @export
#'
ddiff <- function(df1, df2,
                  mismatch = purrr::partial(diffness, scale = FALSE),
                  patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.6,
                  break_penalty = 0.99,
                  perm_penalty = 0.4,
                  penalty_scaling = purrr::partial(ks_scaling, nx = nrow(df1), ny = nrow(df2)),
                  as.list = FALSE, verbose = FALSE) {

  stopifnot(is.data.frame(df1) && is.data.frame(df2))

  stopifnot(perm_penalty >= 0 && perm_penalty <= 1)
  stopifnot(all(patch_penalties >= 0) && all(patch_penalties <= 1))
  stopifnot(break_penalty >= 0)

  # Note that the case length(df1) < length(df2) can be handled by
  # clue::solveLSAP, and then we could add dummy columns to make the number of
  # columns in the result equal to that in df2.

  # The case length(df1) > length(df2) is treated by jgeddes in step 4 of his
  # algorithm.pdf. The niggle with that approach is that we must add dummy
  # columns whose mimatch with any column in the df2 is a fixed cost (i.e.
  # cost_delete) but that is hard to do without complicating the diffness function
  # (e.g. to behave differently when it sees a column tagged as 'dummy').

  if (length(df1) != length(df2))
    stop("Not yet implemented")

  # Construct a nested list of candidate patches between column pairs.
  mismatch_attr <- "mismatch"
  penalty_attr <- "penalty"
  cw_candidates <- columnwise_candidates(df1, df2 = df2,
                                       mismatch = mismatch,
                                       patch_generators = patch_generators,
                                       patch_penalties = patch_penalties,
                                       break_penalty = break_penalty,
                                       penalty_scaling = penalty_scaling,
                                       mismatch_attr = mismatch_attr,
                                       penalty_attr = penalty_attr,
                                       verbose = verbose)

  extract_matrix <- function(attr_name) {
    matrix(purrr::map_dbl(unlist(cw_candidates), .f = function(p) {
      attr(p, attr_name)
    }), nrow = ncol(df1), ncol = ncol(df2), byrow = TRUE)
  }

  m_mismatch <- extract_matrix(mismatch_attr)
  m_penalty <- extract_matrix(penalty_attr)

  if (verbose) {
    cat("mismatch matrix:\n")
    print(m_mismatch)
    cat("penalty matrix:\n")
    print(m_penalty)
  }

  # Sove the assignment problem by applying the Hungarian algorithm to the costs
  # matrix, then convert the solution into a permutation patch.
  soln <- clue::solve_LSAP(m_mismatch + m_penalty, maximum = FALSE)

  # Use 'order' to convert from column indices to a permutation.
  # TODO: if possible use gen_patch_perm here (ncols must be equal)
  perm <- order(as.integer(soln))
  if (identical(perm, 1:ncol(df2)))
    perm_candidate <- patch_identity()
  else
    perm_candidate <- patch_perm(perm)

  # Identify the corresponding columnwise candidate patches and compose them,
  # together with the candidate permutation, to construct the overall candidate.
  patch_list <- c(purrr::map(1:ncol(df1), .f = function(i) {
    cw_candidates[[i]][[soln[i]]]
  }), perm_candidate)
  patch_list <- purrr::discard(patch_list, .p = function(p) {
    methods::is(p, "patch_identity")
  })
  if (length(patch_list) == 0)
    candidate <- patch_identity()
  else
    candidate <- Reduce(compose_patch, rev(patch_list))

  if (verbose) {
    cat("candidate patch:\n")
    print(candidate)
  }

  # Calculate the total cost of doing nothing as the mismatch between the
  # original data frames.
  tc_identity <- mismatch(df1, df2)

  # Calculate the total cost of the candidate as the sum of the total costs of
  # the pairwise candidates, plus the penalty associated with the permutation.
  # Note that this assumes that the mismatch is additive over columns.
  tc_candidate <- sum(purrr::map_dbl(1:ncol(df1), .f = function(i) {
    m_mismatch[i, soln[i]] + m_penalty[i, soln[i]]
  })) + (penalty_scaling(perm_penalty) * sum(perm != 1:ncol(df2)))

  if (verbose) {
    cat(paste("Candidate total cost:", tc_candidate, "\n"))
    cat(paste("Unpatched mismatch:", tc_identity, "\n"))
  }

  # Unless the mismatch reduction exceeds the total candidate cost, do nothing.
  if (tc_identity <= tc_candidate)
    candidate <- patch_identity()

  if (as.list)
    return(decompose_patch(candidate))
  candidate
}
