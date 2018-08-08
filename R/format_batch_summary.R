#' Format a summary of batched synthetic experiments
#'
#' Writes a markdown formatted table containing the summarised batch experiment
#' results to a text file.
#'
#' @param batch_summary
#' A batch output summary matrix of the type returned by the
#' \code{\link{batch_summary}} function.
#' @param filename
#' A file name.
#' @param path
#' A file path.
#' @param tex
#' A logical flag. If \code{TRUE} the table rows are formatted for TeX. Defaults
#' to \code{FALSE}.
#'
#' @seealso \code{\link{batch_experiment}}
#'
#' @keywords internal
#' @export
format_batch_summary <- function(batch_summary, filename, path, tex = FALSE) {

  m <- t(batch_summary)
  is_na_row <- purrr::map_lgl(1:nrow(m), .f = function(i) {
    all(is.na(m[i, ]))
  })
  is_na_col <- purrr::map_lgl(1:ncol(m), .f = function(j) {
    all(is.na(m[, j]))
  })
  m <- m[!is_na_row, !is_na_col]

  format_line <- function(line) { paste("|", paste(line, collapse = " | "), "|") }
  if (tex)
    format_line <- function(line) { paste(paste(line, collapse = " & "), "\\\\") }

  capitaliseFirst <- function(str) {
    paste0(toupper(substring(str, 1, 1)), substring(str, 2, nchar(str)))
  }

  fileConn <- file(file.path(path, filename))

  lines <- format_line(c("Patch Type", colnames(m)))
  lines <- c(lines, format_line(rep("---", times = ncol(m) + 1)))
  lines <- c(lines, purrr::map_chr(1:nrow(m), .f = function(i) {
    format_line(c(capitaliseFirst(rownames(m)[i]), format(m[i, ], digits = 3)))
  }))

  writeLines(lines, fileConn)
  close(fileConn)
}
