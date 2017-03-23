#' Permute a vector according to a cycle
#'
#' Returns the vector \code{v} with its elements permuted according to the given
#' cycle. A cycle of, e.g., c(1, 3, 4) is interpreted as the function
#' \eqn{\sigma} such that \eqn{\sigma(1) = 3}, \eqn{\sigma(3) = 4},
#' \eqn{\sigma(4) = 1} and \eqn{\sigma(x) = x} for all other \eqn{x}.
#'
#' @param v
#' A vector
#' @param cyc
#' A cycle.
#' @return
#' The vector \code{v} with its elements permuted according to the given cycle.
#'
#' @export
#'
#' @examples
#' cycle(1:5, cyc = c(1, 3, 4))
#'
cycle <- function(v, cyc) {

  temp <- v
  rot <- c(cyc[-1], cyc[1])

  temp[cyc] <- temp[rot]
  temp
}

