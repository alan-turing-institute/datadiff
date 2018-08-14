#' Diff two data frames
#'
#' @param df1,df2
#' A pair of data frames.
#' @param alpha
#' A scalar double value specifying a confidence level.
#' @param verbose
#' A logical flag.
#' @param ...
#' Additional arguments passed to the \code{\link{ddiff}} function.
#'
#' @return A composite patch object or, if \code{as.list} is \code{TRUE}, its
#' decomposition as a list of elementary patches.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ks_ddiff(mtcars, mtcars[, 11:1])
#' }
#'
ks_ddiff <- function(df1, df2,
                  alpha = 0.1,
                  verbose = FALSE,
                  ...) {

  # Perform a "first-pass" K-S test.
  ks_reject <- columnwise_ks(df1 = df1, df2 = df2, alpha = alpha)

  ks_accept <- !ks_reject & !is.na(ks_reject)

  if (verbose && any(ks_accept))
    message(paste("K-S test failed to reject at level", alpha, "on columns",
                  paste(which(!ks_reject), collapse = ", ")))

  if (all(ks_accept))
    return(patch_identity())

  ignore_cols <- NULL
  if (any(ks_accept))
    ignore_cols <- which(ks_accept)

  ## TODO:
  # If the K-S test rejects the null on only one column (in the "target" data
  # frame), discard the first-pass K-S test altogether and run ddiff without
  # ignoring any columns. This is a conservative approach, given that it is
  # unclear what else to do here.
  if (ncol(df2) - sum(ks_accept) == 1)
    ignore_cols <- NULL

  ddiff(df1 = df1, df2 = df2, ignore_cols = ignore_cols, verbose = verbose, ...)
}
