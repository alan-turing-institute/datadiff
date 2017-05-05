#' Generate an identity patch
#'
#' Generates an identity patch object with mismatch method attached as an
#' attribute.
#'
#' @param df1
#' A data frame.
#' @param df2
#' A data frame.
#' @param mismatch
#' Mismatch method. The default is (unscaled) \code{\link{diffness}}.
#' @param ...
#' Additional arguments are ignored.
#'
#' @return An identity \code{patch} object with mismatch method attached as an
#' attribute.
#'
#' @seealso \code{\link{optim}}
#'
#' @export
gen_patch_identity <- function(df1, df2,
                               mismatch = purrr::partial(diffness, scale = FALSE)
                               , ...) {

  ret <- patch_identity()
  attr(ret, which = "mismatch") <- mismatch
  ret
}
