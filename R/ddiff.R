#' Diff two data frames
#'
#' @description
#' Given two data frames and a collection of patch generator functions, with
#' associated (numeric) penalties, this function attempts to identify a
#' sequence of transformations to best explain the difference and returns the
#' result as a (composite) patch.
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
#' @param permute_penalty
#' The penalty associated with a permutation patch.
#' @param penalty_scaling
#' A function to be used to scale the penalty associated with each patch.
#' Defaults to \code{\link{ks_scaling}}.
#' @param insert_col_name
#' The column name used for inserted columns, if \code{df1} has fewer columns
#' than \code{df2}. Defaults to 'INSERT'.
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
                  mismatch = diffness,
                  patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.6,
                  break_penalty = 0.99,
                  permute_penalty = 0.4,
                  penalty_scaling = purrr::partial(ks_scaling, nx = nrow(df1),
                                                   ny = nrow(df2)),
                  insert_col_name = "INSERT",
                  as.list = FALSE, verbose = FALSE) {

  stopifnot(is.data.frame(df1) && is.data.frame(df2))
  stopifnot(permute_penalty >= 0 && permute_penalty <= 1)
  stopifnot(all(patch_penalties >= 0) && all(patch_penalties <= 1))
  stopifnot(break_penalty >= 0)

  # Construct a nested list of candidate patches between column pairs.
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
    }), nrow = ncol(df1), ncol = ncol(df2), byrow = TRUE)
  }
  m_mismatch <- extract_matrix(mismatch_attr)
  m_penalty <- extract_matrix(penalty_attr)
  if (verbose) {
    cat("mismatch matrix:\n"); print(m_mismatch)
    cat("penalty matrix:\n"); print(m_penalty)
  }

  # Solve the problem of assigning columns in df1 to those in df2 with minimum
  # cost. The solution has length max(ncol(df1), ncol(df2)).
  soln <- solve_pairwise_assignment(m_mismatch + m_penalty, maximum = FALSE)

  column_is_unassigned <- soln > ncol(df2)
  column_is_not_assigned_to <- !(1:length(soln) %in% soln[1:ncol(df1)])

  # Calculate the total cost (mismatch + penalty) of the candidate.
  # Note that this assumes that the mismatch is additive over columns.
  cw_candidates_total_cost <- sum(purrr::map_dbl(1:ncol(df1), .f = function(i) {
    if (column_is_unassigned[i])
      return(0) # Unassigned columns from df1 will be deleted (without cost).
    m_mismatch[i, soln[i]] + m_penalty[i, soln[i]]
  }))

  # Identify the permuation corresponding to the solution, using 'order' to go
  # from column indices to a permutation, and calculate the associated penalty.
  perm <- order(soln[!column_is_unassigned])
  candidate_perm_penalty <- penalty_scaling(permute_penalty) *
    sum(perm != 1:ncol(df2))

  ## Compare the total cost of the (composed) candidate vs. doing nothing.
  tc_candidate <- cw_candidates_total_cost + candidate_perm_penalty
  tc_identity <- mismatch(df1, df2)

  if (verbose) {
    cat(paste("Candidate total cost:\t", tc_candidate, "\n"))
    cat(paste("Unpatched mismatch:\t", tc_identity, "\n"))
  }

  # Unless the mismatch reduction exceeds the total candidate cost, do nothing.
  if (tc_identity <= tc_candidate)
    return(patch_identity())

  ## Now that the candidate has been accepted, construct the patch itself.

  # Identify the columnwise candidate patches.
  p_columnwise <- purrr::map(1:ncol(df1), .f = function(i) {
    if (column_is_unassigned[i])
      return(patch_identity()) # Do nothing to columns which will be deleted.
    cw_candidates[[i]][[soln[i]]]
  })

  # Identify any insert or delete patches (at most one list will be non-trivial).
  identify_insert_patches <- function(column_is_not_assigned_to) {
    if (!any(column_is_not_assigned_to))
      return(patch_identity())
    insert_col_data <- data.frame(rep(NA, nrow(df2)))
    colnames(insert_col_data) <- insert_col_name
    purrr::map(which(column_is_not_assigned_to), .f = function(i) {
      patch_insert(as.integer(perm[i] - 1), data = insert_col_data)
    })
  }
  identify_delete_patches <- function(column_is_unassigned) {
    if (!any(column_is_unassigned))
      return(patch_identity())
    purrr::map(sort(which(column_is_unassigned), decreasing = TRUE),
               .f = patch_delete)
  }
  p_insert <- identify_insert_patches(column_is_not_assigned_to)
  p_delete <- identify_delete_patches(column_is_unassigned)

  # Construct the permutation patch.
  if (identical(perm, 1:ncol(df2)))
    p_permute <- patch_identity()
  else
    p_permute <- patch_perm(perm)

  # Construct the candidate as a list of patches (for composition).
  # Note that the order is important here: columnwise patches must be applied
  # before any insertions/deletions but the permutation must go them.
  # Note: we compose even when as.list is TRUE to return only elementary patches.
  patch_list <- c(p_columnwise, p_insert, p_delete, p_permute)
  p <- simplify_patch(Reduce(compose_patch, rev(patch_list)))

  if (verbose) {
    cat("candidate patch list:\n")
    print(p)
  }

  if (as.list)
    return(decompose_patch(p))
  p
}
