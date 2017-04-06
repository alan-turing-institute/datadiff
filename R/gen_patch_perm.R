#' Generate an optimal permutation patch
#'
#' Computes the permutation of columns, expressed as a \code{patch_perm} patch,
#' that minimises the mismatch between the patched \code{df1} and \code{df2},
#' including a permutation cost. Currently the permutation cost is ignored.
#'
#' This function uses an assignment problem solver from the package
#' \code{lpSolve}.
#'
#' @param df1 A data frame
#' @param df2 A data frame
#' @param cost A "permutation cost"
#'
#' @return A \code{patch_perm} object.
#'
#' @export
gen_patch_perm <- function(df1, df2, cost = 0) {
    
    ## The approach here is:
    ## 1. Pre-compute the pairwise set of mismatches;
    ## 2. Find the lowest-mismatch pairing

    n_cols <- ncol(df1)
    if (n_cols != ncol(df2)) stop("df1 and df2 must have the same number of columns")

    ## Create a matrix of all pairwise mismatches
    mismatches <- purrr::map2_dbl(rep(1L:n_cols, times = n_cols),
                                  rep(1L:n_cols, each = n_cols),
                                  function(i, j) diffness(df1[, i], df2[, j]))

    mismatches <- array(mismatches, dim = c(n_cols, n_cols))

    ## Find the optimal match (as a matrix of 0s and 1s)
    soln <- lpSolve::lp.assign(mismatches)

    ## Recover the permutation if the mismatch is worth the cost
    mismatch_improvement <- sum(diag(mismatches)) -  soln$objval

    if (mismatch_improvement > cost) {
        patch_perm(apply(soln$solution, 1, function(v) which(v != 0)))
    } else {
        patch_identity()
    }
}
         
    
