#' Compute cost scale factor
#'
#' The scale factor decreases the cost in proportion to the inverse square root
#' of the number of rows. This means that, when comparing against an unscaled
#' diffness, small diffnesses count more the more data points there are.
#'
#' TODO: explain the origin of the scale factor.
#'
#' @param nx,ny The lengths of the vectors
#' @return A number
#'
#' @export
cost_scale <- function(nx, ny) {
  sqrt((nx + ny) / (nx * ny))
}
