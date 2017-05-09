#' TODO. Scale a numeric penalty
#'
#' The scale factor decreases the cost in proportion to the inverse square root
#' of the number of rows. This means that, when comparing against an unscaled
#' diffness, small diffnesses count more the more data points there are.
#'
#' TODO: explain the origin of the scale factor incl. reference to:
#' https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test#Two-sample_Kolmogorov.E2.80.93Smirnov_test
#'
#' @param penalty A numeric penalty
#' @param nx,ny The lengths of the vectors
#' @return A number
#'
#' @export
ks_scaling <- function(penalty, nx, ny) {

  critical_value <- function(alpha) { sqrt(-log(alpha / 2) / 2) }
  critical_value(1 - penalty) * sqrt((nx + ny) / (nx * ny))
}
