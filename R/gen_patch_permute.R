#' Generate an optimal permutation patch
#'
#' Computes the permutation of columns, expressed as a \code{patch_permute} object,
#' that minimises the mismatch between the data frame \code{df2} and the patched
#' \code{df1}.
#'
#' This function uses an assignment problem solver from the package
#' \code{lpSolve}.
#'
#' @param df1
#' A data frame.
#' @param df2
#' A data frame.
#' @param mismatch
#' Mismatch method. The default is \code{\link{diffness}}.
#' @param ...
#' Additional arguments are ignored.
#'
#' @return A \code{patch_permute} object.
#'
#' @export
#'
#' @examples
#' gen_patch_permute(mtcars, mtcars[, 11:1])
#'
gen_patch_permute <- function(df1, df2, mismatch = diffness, ...) {

    ## The approach here is:
    ## 1. Pre-compute the pairwise set of mismatches;
    ## 2. Find the lowest-mismatch pairing

    n_cols <- ncol(df1)
    if (n_cols != ncol(df2))
      stop("df1 and df2 must have the same number of columns")

    ## Create a matrix of all pairwise mismatches
    mismatches <- purrr::map2_dbl(rep(1L:n_cols, times = n_cols),
                                  rep(1L:n_cols, each = n_cols),
                                  function(i, j) mismatch(df1[, i], df2[, j]))

    mismatches <- array(mismatches, dim = c(n_cols, n_cols))

    ## Find the optimal match (as a matrix of 0s and 1s)
    soln <- lpSolve::lp.assign(mismatches)

    perm <- apply(soln$solution, 1, function(v) which(v != 0))
    if (identical(perm, 1:n_cols))
      return(patch_identity())
    patch_permute(perm)
}


