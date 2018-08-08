#' Scale a penalty linearly
#'
#' The scale factor is the square root of the sum of the lengths divided by the
#' product of the lengths.
#'
#' @param penalty A numeric penalty
#' @param nx,ny The lengths of the vectors
#'
#' @return A (numeric) linearly-scaled penalty.
#'
#' @seealso \code{\link{ks_scaling}}
#'
#' @export
#'
#' @examples
#' linear_scaling(0.5, nx = 100, ny = 100)
#'
linear_scaling <- function(penalty, nx, ny) {

  penalty * sqrt((nx + ny) / (nx * ny))
}
