#' Scale a numeric penalty
#'
#' The scale factor decreases the cost in proportion to the inverse square root
#' of the number of rows. This means that, when comparing against an unscaled
#' diffness, small diffnesses count more the more data points there are.
#'
#' TODO: explain the origin of the scale factor.
#'
#' @param penalty A numeric penalty
#' @param nx,ny The lengths of the vectors
#' @return A number
#'
#' @export
scale_penalty <- function(penalty, nx, ny) {
  penalty * sqrt((nx + ny) / (nx * ny))
}
