#' Generate an optimal permutation patch
#'
#' Computes the permutation of columns, expressed as a \code{patch_perm} patch,
#' that minimises the mismatch between the patched \code{df1} and \code{df2},
#' including a permutation cost. Currently the permutation cost is ignored.
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

    mismatches <- array(dim = c(n_cols, n_cols))

    for (i in 1L:n_cols) {
        for (j in i:n_cols) {
            mismatches[i, j] <- diffness(df1[ ,i], df1[, j])
            mismatches[j, i] <- mismatches[i, j]
        }
    }

    mismatches
}
         
    
