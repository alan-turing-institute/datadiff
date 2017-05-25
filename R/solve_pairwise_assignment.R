#' Solve the linear sum assignment problem between column pairs
#'
#' Helper function for \code{\link{ddiff}}. Solves the optimal assignment
#' problem,  based on the given costs matrix, via the Hungarian algorithm using
#' the implementation in \code{\link{solve_LSAP}} from the \code{clue} package.
#'
#' If it is not already square, the costs matrix \code{m} is augmented with rows
#' or columns of zeros to obtain a square matrix. In case
#' \code{ncol(m) < nrow(m)}, rows in \code{m} which are not assigned to any
#' column in \code{m} are instead assigned to the augmented rows. Similarly, in
#' case \code{nrow(m) < ncol(m)}, columns in \code{m} to which no row is
#' assigned are instead assigned from the augmented columns.
#'
#' @param m
#' A costs matrix with nonnegative entries.
#' @param maximum
#' A logical flag indicating whether to minimise or maximise the sum of assigned
#' costs.
#' @param verbose
#' A logical flag.
#'
#' @return An integer vector containing the solution of the assignment problem.
#' The \code{i}th element is the index of the column to which the \code{i}th row
#' is assigned.
#'
#' @export
#'
solve_pairwise_assignment <- function(m, maximum = FALSE, verbose = FALSE) {

  # The reasoning behind the augmentation of m with zeros (in the case of a
  # non-square matrix) is based on the following identities:
  # (Here soln refers to the solution obtained from clue::solve_LSAP)
  #
  # 1. soln(m) = rev(soln(m'))
  # 2. if nrow(m) < ncol(m) then soln(m) = soln(M)|_{1:nrow(m)} where
  #    M is the matrix obtained from m by adding rows of zeros.
  # 3. if ncol(m) < nrow(m) then soln(m') = rev(soln(M)|_{1:ncol(m)}) where
  #    M is the matrix obtained from m by adding columns of zeros.

  # If necessary, augment the matrix with zeros to obtain a square matrix.
  M <- m
  if (nrow(m) < ncol(m))
    M <- rbind(m, t(replicate(ncol(m) - nrow(m), rep(0, ncol(m)))))
  if (ncol(m) < nrow(m))
    M <- cbind(m, replicate(nrow(m) - ncol(m), rep(0, nrow(m))))

  # Solve the assignment problem by applying the Hungarian algorithm.
  soln <- clue::solve_LSAP(M, maximum = maximum)

  # Sanity check the effect of augmenting the matrix.
  if (nrow(m) < ncol(m)) {
    is_assigned_to <- 1:ncol(m) %in% soln[1:nrow(m)]

    # Columns in m which are not assigned to from rows in m ought to be assigned
    # to from the augmented rows in M.
    if (!setequal((1:ncol(m))[!is_assigned_to], soln[(nrow(m) + 1):ncol(m)]))
      stop("'Not assigned to' column(s) do not match augmented row(s)")

    # Standardise the order of "not assigned to" columns.
    soln[(nrow(m) + 1):ncol(m)] <- sort(soln[(nrow(m) + 1):ncol(m)])
  }
  if (ncol(m) < nrow(m)) {
    is_assigned <- soln <= ncol(m)

    # Rows in m which are not assigned to columns in m ought to be assigned
    # to the augmented columns in M.
    if (!setequal(soln[!is_assigned], (ncol(m) + 1):nrow(m)))
      stop("Unassigned row(s) do not match augmented column(s)")

    # Standardise the order of unassigned rows.
    soln[!is_assigned] <- sort(soln[!is_assigned])
  }

  if (verbose)
    print(soln)
  as.integer(soln)
}
