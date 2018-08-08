#' Split a data frame into two pieces
#'
#' @param df
#' A data frame.
#' @param split
#' A number in the unit interval specifying the splitting ratio.
#'
#' @return A list containing two data frames.
#'
#' @export
#'
#' @examples
#' split_data(head(mtcars), split = 1/2)
#'
split_data <- function(df, split) {
  rows <- sort(sample.int(nrow(df), size = split * nrow(df), replace = FALSE))
  list(df[rows, ], df[-rows, ])
}
