#' Generate a recode patch
#'
#' Generates a \code{patch_recode} object whose 'encoding' parameter has been
#' selected with the aim of minimising the mismatch between the specified
#' columns after application of the patch to \code{df1}.
#'
#' @param df1
#' A data frame.
#' @param col1
#' A column identifier (integer or string column name) with length 1.
#' @param df2
#' A data frame.
#' @param col2
#' A column identifier (integer or string column name) with length 1. By default
#' this takes the value of \code{col1}.
#' @param ...
#' Additional arguments passed to the \code{patch_recode} function.
#'
#' @return A \code{patch_recode} object.
#'
#' @seealso \code{\link{patch_recode}}
#'
#' @export
gen_patch_recode <- function(df1, col1, df2, col2 = col1, ...) {

  stopifnot(is_compatible_columns(col1, df1) && length(col1) == 1)
  stopifnot(is_compatible_columns(col2, df2) && length(col2) == 1)

  v1 <- df1[[col1]]
  v2 <- df2[[col2]]

  stopifnot(sum(!is.na(v1)) != 0 && sum(!is.na(v2)) != 0)

  if (is.double(v1) || is.double(v2))
    stop("Encodings require discrete data")

  f1 <- as.factor(v1)
  f2 <- as.factor(v2)
  lev1 <- levels(f1)
  lev2 <- levels(f2)

  if (length(lev2) < length(lev1))
    stop("Insufficient target codes")

  t1 <- tabulate(f1)
  t2 <- tabulate(f2)

  # Apply the Hungarian algorithm to solve the assignment problem.
  x <- outer(t1, t2, FUN = purrr::compose(abs, `-`))
  soln <- clue::solve_LSAP(x, maximum = FALSE)

  # Convert the solution into a recode patch.
  encoding <- lev2[soln]
  if (!is.factor(v2))
    encoding <- methods::as(encoding, class(v2))
  names(encoding) <- lev1

  patch_recode(col1, encoding = encoding, one_to_one = TRUE)
}
