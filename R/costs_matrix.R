#' Compute a costs matrix
#'
#' @description
#' Generates a candidate patch for each pair of columns (one each from
#' \code{df1} & \code{df2}), calculates the corresponding costs and returns them
#' in a matrix.
#'
#' This function is included solely for the purpose of assisting demonstration
#' of the datadiff package and contains code which is repeated elsewhere.
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
#' @param permute_penalty
#' The penalty associated with a permutation patch. This penalty is scaled, using
#' the \code{penalty_scaling} argument, and multiplied by the number of columns
#' whose index is changed by the permutation.
#' @param break_penalty
#' The penalty associated with a break patch.
#' @param penalty_scaling
#' A function to be used to scale the penalty associated with each patch.
#' Defaults to \code{\link{ks_scaling}}.
#' @param square
#' A logical flag. If \code{TRUE}, and \code{df1} & \code{df2} have differing
#' numbers of columns, rows will be inserted or deleted in the cost matrix,
#' corresponding to insertions/deletions of columns in \code{df1} (see
#' \code{\link{ddiff}}), to obtain a square matrix.
#' @param verbose
#' A logical flag.
#'
#' @return A numeric matrix.
#'
#' @export
#'
costs_matrix <- function(df1, df2 = df2,
                         mismatch = diffness,
                         patch_generators = list(gen_patch_transform),
                         patch_penalties = 0.6,
                         permute_penalty = 0.1,
                         break_penalty = 0.99,
                         penalty_scaling = purrr::partial(ks_scaling, nx = nrow(df1),
                                                          ny = nrow(df2)),
                         square = TRUE,
                         verbose = FALSE) {

  stopifnot(is.data.frame(df1) && is.data.frame(df2))
  stopifnot(permute_penalty >= 0 && permute_penalty <= 1)
  stopifnot(all(patch_penalties >= 0) && all(patch_penalties <= 1))
  stopifnot(break_penalty >= 0)

  mismatch_attr <- "mismatch"; penalty_attr <- "penalty"
  cw_candidates <- columnwise_candidates(df1, df2 = df2,
                                         mismatch = mismatch,
                                         patch_generators = patch_generators,
                                         patch_penalties = patch_penalties,
                                         break_penalty = break_penalty,
                                         penalty_scaling = penalty_scaling,
                                         mismatch_attr = mismatch_attr,
                                         penalty_attr = penalty_attr,
                                         verbose = verbose)

  # Extract the costs matrix (from attributes attached to the candidates).
  extract_matrix <- function(attr_name) {
    matrix(purrr::map_dbl(unlist(cw_candidates), .f = function(p) {
      attr(p, attr_name)
    }), nrow = ncol(df1), ncol = ncol(df2), byrow = TRUE) }
  m_mismatch <- extract_matrix(mismatch_attr)
  m_penalty <- extract_matrix(penalty_attr)

  add_permute_penalty <- function(m) {
    m + penalty_scaling(permute_penalty) * (1 - diag(nrow(m))) }

  if (ncol(df1) == ncol(df2))
    m_penalty <- add_permute_penalty(m_penalty)

  if (!square)
    return(m_mismatch + m_penalty)

  # Solve the problem of assigning columns in df1 to those in df2 with minimum
  # cost. The solution has length max(ncol(df1), ncol(df2)).
  soln <- solve_pairwise_assignment(m_mismatch + m_penalty, verbose = verbose)

  # Handle the possibility that ncol(df2) != ncol(df1).
  column_is_unassigned <- soln > ncol(df2)
  column_is_not_assigned_to <- !(1:length(soln) %in% soln[1:ncol(df1)])

  # In case ncol(df1) < ncol(df2), identify the requisite insert patch(es).
  if (any(column_is_not_assigned_to)) {

    # Update each cost matrix (with a new row corresponding to each column in
    # df2 which is not assigned to).
    update_matrix <- function(m, i) {
      ret <- rbind(m[0:(i - 1), ], (1 - diag(ncol(df2)))[i, ])
      if (i <= nrow(m))
        ret <- rbind(ret, m[i:nrow(m), ])
      ret
    }
    sink <- purrr::map(which(column_is_not_assigned_to), .f = function(i) {
      m_mismatch <<- update_matrix(m_mismatch, i)
      m_penalty <<- update_matrix(m_penalty, i)
    })
  }
  # In case ncol(df1) > ncol(df2), identify the requisite delete patch(es).
  if (any(column_is_unassigned)) {

    # Update each cost matrix (by removing those rows corresponding to columns
    # in df1 which are unassigned).
    update_matrix <- function(m, i) { m[-i, ] }
    sink <- purrr::map(sort(which(column_is_unassigned), decreasing = TRUE),
                           .f = function(i) {
                             m_mismatch <<- update_matrix(m_mismatch, i)
                             m_penalty <<- update_matrix(m_penalty, i)
                           })
  }

  # Include the permutation penalties (now that the penalties matrix is square).
  if (any(column_is_not_assigned_to) || any(column_is_unassigned))
    m_penalty <- add_permute_penalty(m_penalty)

  m_mismatch + m_penalty
}
