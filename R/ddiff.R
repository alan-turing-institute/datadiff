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
#' @param verbose
#' A logical flag.
#'
#' @return A list of patch objects, or their composition if \code{composed} is
#' \code{TRUE}.
#'
#' @export
#'
ddiff <- function(df1, df2, cost_permute, cost_transform, cost_break = -0.01,
                  as.list = FALSE, verbose = FALSE) {


  stopifnot(is.data.frame(df1) && is.data.frame(df2))

  stopifnot(cost_permute >= 0 && cost_permute <= 1)
  stopifnot(cost_transform >= 0 && cost_transform <= 1)
  ## TEMP! temporary negative cost_break for testing.
  #stopifnot(cost_break >= 0 && cost_break <= 1)

  cost_scale_factor <- cost_scale(nrow(df1), nrow(df2))

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

  # Construct matrices to hold the pairwise costs and the diffs.
  m_costs <- matrix(NA, nrow = ncol(df1), ncol = ncol(df2))
  m_diffs <- m_costs

  # Construct a nested list of candidate transformation (or break) patches and
  # simultaneously fill the costs & diffs matrices. Note that the costs + diffs
  # matrix cannot be processed until all elements have been calculated, after
  # which we must be able to recover the patches involved in those calculations.
  candidate_tx <- purrr::map(1:ncol(df1), .f = function(i) {
    purrr::map(1:ncol(df2), .f = function(j) {

      # Determine the appropriate type of transformation patch.
      if (is.double(df1[[i]]))
        gen_patch_tx <- gen_patch_affine
      else
        gen_patch_tx <- gen_patch_recode

      # Construct a list of all candidate transformation patches for this column
      # pair, and corresponding costs, in order of preference (in case of a tie).
      tx_list <- list(
        list(patch_identity(), 0),
        list(tryCatch(
          gen_patch_tx(df1 = df1, col1 = i, df2 = df2, col2 = j),
          error = function(e) {
            if (verbose)
              warning(paste0("gen_patch_tx at [", i, ", ", j,
                             "] returned error:\n", conditionMessage(e), "\n"))
            NULL
          }), cost_scale_factor * cost_transform),
        list(gen_patch_break(df1 = df1, col1 = i, df2 = df2, col2 = j),
             cost_scale_factor * cost_break)
      )
      tx_list <- purrr::discard(tx_list, .p = function(x) { is.null(x[[1]]) })

      # Compute the corresponding diffnesses.
      sink <- purrr::map(1:length(tx_list), .f = function(k) {
        p <- tx_list[[k]][[1]]
        d <- diffness(p(df1)[[i]], df2[[j]], scale = FALSE)
        tx_list[[k]] <<- c(tx_list[[k]], d)
      })

      # Identify which is the best candidate in the list.
      column_costs <- purrr::map_dbl(tx_list, .f = function(x) {
        sum(unlist(purrr::discard(x, .p = is_patch, allow_composed = TRUE)))
      })
      best <- min(which(column_costs == min(column_costs)))
      tx <- tx_list[[best]]

      # Fill the costs and diffs matricess and return the best candidate patch.
      m_costs[i, j] <<- tx[[2]]
      m_diffs[i, j] <<- tx[[3]]
      tx[[1]]
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
  soln <- clue::solve_LSAP(m_costs + m_diffs, maximum = FALSE)
  # Use order to convert from column indices to perm.
  perm <- order(as.integer(soln))

  # TODO: remove any superfluous identity patches in the selected_tx list.
  # Identify the corresponding transformation patches and compose them, together
  # with the permutation patch, to construct the overall candidate patch.
  patch_list <- c(purrr::map(1:ncol(df1), .f = function(i) {
    candidate_tx[[i]][[soln[i]]]
  }), patch_perm(perm))
  patch_list <- purrr::discard(patch_list, .p = function(p) {
    identical(patch_type(p, short = TRUE), "identity")
  })
  candidate <- Reduce(compose_patch, rev(patch_list))

  if (verbose) {
    cat("candidate patch:\n")
    print(candidate)
  }

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
  candidate_diffness <- diffness(candidate(df1), df2, scale = FALSE)
  candidate_transform_costs <- sum(candidate_costs)
  candidate_permute_costs <- prod(cost_scale_factor, cost_permute,
                                  sum(perm != 1:ncol(df1)))
  candidate_total <- sum(candidate_diffness, candidate_transform_costs,
                         candidate_permute_costs)

  # Calculate the total cost of doing nothing as the (unscaled) diffness between
  # the original data frames.
  identity_total <- diffness(df1, df2, scale = FALSE)

  if (verbose) {
    cat("Candidate total costs = diffness + txs cost + permute cost:\n")
    cat(paste(candidate_total, "=", candidate_diffness, "+",
              candidate_transform_costs, "+", candidate_permute_costs, "\n"))
    cat(paste("Unpatched mismatch:", identity_total, "\n"))
  }

  # Unless the mismatch reduction exceeds the total candidate cost, do nothing.
  if (identity_total <= candidate_total)
    candidate <- patch_identity()

  # Add the (unscaled) cost parameters to the return value as attributes for
  # reproducibility.
  attr(candidate, "cost_permute") <- cost_permute
  attr(candidate, "cost_transform") <- cost_transform
  attr(candidate, "cost_break") <- cost_break

  if (as.list)
    return(decompose_patch(candidate))
  candidate
}
