#' Diff two data frames
#'
#' @description
#' TODO.
#'
#' @param df1,df2
#' A pair of data frames.
#' @param cost_permute
#' A number in the unit interval representing the cost of a permute patch
#' operation.
#' @param cost_transform
#' A number in the unit interval representing the cost of a transformation patch
#' (i.e. affine for continuous data, recode for categorical) operation.
#' @param cost_break
#' A number in the unit interval representing the cost of a break patch
#' operation.
#' @param as.list
#' A logical flag. If \code{TRUE} the return value is a list of patches.
#' Otherwise a composite patch object is returned.
#'
#' @return A list of patch objects, or their composition if \code{composed} is
#' \code{TRUE}.
#'
#' @export
#'
ddiff <- function(df1, df2, cost_permute, cost_transform, cost_break = 0.99,
                  as.list = FALSE, verbose = FALSE) {

  ### BUGFIX: either scale diffness, or the cost, but not both.
  # Implement the ks-test with the contant being the cost parameter and
  # include the scale factor. (Or do something equivalent to this.)

  stopifnot(is.data.frame(df1) && is.data.frame(df2))

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

  # Scale the costs.
  cost_scale_factor <- cost_scale(nrow(df1), nrow(df2))
  cost_permute <- cost_permute * cost_scale_factor
  cost_transform <- cost_transform * cost_scale_factor
  cost_break <- cost_break * cost_scale_factor

  # Construct matrices to hold the pairwise costs and the diffs.
  m_costs <- matrix(NA, nrow = ncol(df1), ncol = ncol(df2))
  m_diffs <- m_costs

  # Construct a nested list of candidate transformation (or break) patches and
  # simultaneously fill the costs matrix. This is necessary since the costs
  # matrix cannot be processed until all elements have been calculated, after
  # which we must be able to recover the patches involved in those calculations.
  candidate_tx <- purrr::map(1:ncol(df1), .f = function(i) {
    purrr::map(1:ncol(df2), .f = function(j) {

      # Calculate the column cost of doing nothing as the (unscaled) diffness.
      cd_id <- diffness(df1[[i]], df2[[j]], scale = FALSE)
      cc_id <- cd_id + 0 # column cost of doing nothing is zero.

      # Identify the best transformation patch, if any exists.
      if (is.double(df1[[i]]))
        gen_patch_tx <- gen_patch_affine
      else
        gen_patch_tx <- gen_patch_recode
      tx_patch <- tryCatch(
        gen_patch_tx(df1 = df1, col1 = i, df2 = df2, col2 = j),
        error = function(e) { e })

      # If no transformation is possible, return a break patch.
      if (inherits(tx_patch, "error")) {
        # TODO: consider making this the diffness plus cost_break for consistency.
        m_costs[i, j] <<- cost_break
        m_diffs[i, j] <<- 0
        return(gen_patch_break(df1 = df1, col1 = i, df2 = df2, col2 = j))
      }

      # Calculate the column cost of transforming the data as the (unscaled)
      # diffness after transformation, plus the fixed cost of a transformation.
      cd_tx <- diffness(tx_patch(df1)[[i]], df2[[j]], scale = FALSE)
      cc_tx <- cd_tx + cost_transform

      # If the minimum column cost is the break cost, return a break patch.
      if (cost_break < min(cc_id, cc_tx)) {
        m_costs[i, j] <<- cost_break
        m_diffs[i, j] <<- 0
        return(gen_patch_break(df1 = df1, col1 = i, df2 = df2, col2 = j))
      }

      # If the minimum column cost is the transformed diffness plus transform cost,
      # return the transformation patch.
      if (cc_tx < cc_id) {
        m_costs[i, j] <<- cost_transform
        m_diffs[i, j] <<- cd_tx
        return(tx_patch)
      }

      # Otherwise (i.e. if the minimum column cost is to do nothing) return the
      # identity patch.
      m_costs[i, j] <<- 0
      m_diffs[i, j] <<- cd_id
      patch_identity()
    })
  })

  if (verbose) {
    cat("costs matrix:\n")
    print(m_costs)
    cat("diffs matrix:\n")
    print(m_diffs)
  }


  # Sove the assignment problem by applying the Hungarian algorithm to the costs
  # matrix, then convert the solution into a permutation patch.
  soln <- clue::solve_LSAP(m_diffs + m_costs, maximum = FALSE)
  # Use order to convert from col indices to perm.
  perm <- order(as.integer(soln))
  p_perm <- patch_perm(perm)

  if (verbose) {
    cat("candidate permutation:\n")
    print(p_perm)
  }

  # TODO: remove any superfluous identity patches in the selected_tx list.
  # Identify the corresponding transformation patches and compose them, together
  # with the permutation patch, to construct the overall candidate patch.
  patch_list <- c(purrr::map(1:ncol(df1), .f = function(i) {
    candidate_tx[[i]][[soln[i]]]
  }), p_perm)

  # TODO: move this to a compose_patch function (which does the rev).
  p <- Reduce(compose_patch, rev(patch_list))

  # Calculate the total cost of doing nothing as the (unscaled) diffness between
  # the original data frames.
  tc_id <- diffness(df1, df2, scale = FALSE)

  # Calculate the total cost of the candidate patch. Here cost_permute is scaled
  # by the number of columns _moved_ by the permutation. Therefore the identity
  # permutation has no cost.
  # Note that some scaling by the number of columns in df1 is essential (if we
  # want cost_permute to have the same 'scale' as the other cost parameters, i.e.
  # in the range [0,1]) since the (unscaled) diffness between two data frames is
  # a number in the interval [0, max(ncol(df1), ncol(df2))].

  # Include the costs associated with the transformations in the candidate.
  candidate_costs <- purrr::map_dbl(1:ncol(df1), .f = function(i) {
    m_costs[i, soln[i]]
  })
  tc_p <- diffness(p(df1), df2, scale = FALSE) + sum(candidate_costs)
    cost_permute * sum(perm != 1:ncol(df1))

  # If the overall reduction in the mismatch does not exceed cost_permute,
  # do nothing.
  if (tc_id <= tc_p)
    p <- patch_identity()

  # Add the cost parameters to the return value as attributes for
  # reproducibility.
  attr(p, "cost_permute") <- cost_permute
  attr(p, "cost_transform") <- cost_transform
  attr(p, "cost_break") <- cost_break

  if (as.list)
    return(decompose_patch(p))
  p
}
