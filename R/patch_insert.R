#' \code{patch_insert} S3 class constructor
#'
#' @description
#' S3 class \code{patch_insert} which extends the \code{patch} and
#' \code{function} classes to represent a transformation of a tabular dataset
#' by inserting one or more new columns.
#'
#' When applied to a data frame, a \code{patch_insert} calls the
#' \code{dplyr::bind_cols} function to produce its result. As such, it is that
#' function which determines how column name conflicts are handled. As of
#' \code{dplyr} v0.7, this is done with a call to \code{tibble::repair_names}.
#'
#' @param insertion_point
#' A scalar column identifier. The new columns will be inserted immediately
#' after this column.
#' @param data
#' A data frame containing the new column data.
#'
#' @return A \code{patch_insert} object.
#'
#' @export
#'
#' @seealso \code{\link{is_valid_columns}} \code{\link{is_compatible_columns}}
#'
#' @examples
#' head(mtcars)
#' p <- patch_insert("gear", data = mtcars[2:4])
#' p <- patch_insert(0L, data = mtcars[2:4])
#'
#' # The following are equivalent:
#' head(apply_patch(mtcars, p))
#' head(p(mtcars))
#'
#' # Attempting to apply a patch to an incompatible data frame throws an error.
#' \dontrun{
#' p <- patch_insert(22L, data = mtcars[2:4])
#' p(mtcars)
#' }
patch_insert <- function(insertion_point, data) {

  # Check the given parameters are appropriate for the insert patch type.
  stopifnot(length(insertion_point) == 1)
  if (insertion_point != 0)
    stopifnot(is_valid_columns(insertion_point))

  stopifnot(is.data.frame(data))

  # Construct the patch object as a closure.
  obj <- function(df) {

    # Check for compatability between the data frame and the parameters.
    if (insertion_point != 0)
      stopifnot(is_compatible_columns(insertion_point, df))
    stopifnot(nrow(data) == nrow(df))

    if (is.character(insertion_point))
      insertion_point <- match(insertion_point, names(df))

    # Transform the data frame according to the parameters.
    post <- seq.int(insertion_point + 1, length(df))
    if (insertion_point == length(df))
      post <- 0
    ret <- dplyr::bind_cols(df[0:insertion_point], data, df[post])

    stopifnot(is.data.frame(ret))
    ret
  }

  class(obj) <- c("patch_insert", "patch", "function")
  obj
}

#' Randomly sample from a distribution of insert patches
#'
#' @param df
#' A data frame
#' @param rdist
#' A function for randomly generating the data in the inserted column. Must
#' contain an argument \code{n} for the number of samples. Defaults to the
#' standard Normal distribution function \code{rnorm}.
#' @param colname
#' The inserted column name. Defaults to "INSERTED". If a column with that name
#' already exists, the column will be named "INSERTEDx" where \code{x} is an
#' integer which increments each time a column with the duplicated name is
#' inserted.
#' @param exclude_cols
#' An integer vector of column indices to be excluded from the set of possible
#' target columns for the inserted column in the returned patch.
#' @param seed
#' A random seed.
#' @param ...
#' Additional arguments passed to the \code{rdist} function.
#'
#' @export
#'
#' @examples
#' p <- sample_patch_insert(mtcars, mean = 10, sd = 4)
#' head(p(mtcars))
#'
#' # Draw the column data from a given distribution:
#' p <- sample_patch_insert(mtcars, rdist = rexp, rate = 2)
#' head(p(mtcars))
#'
#' # Draw the column data from a discrete distribution:
#' rdist <- function(n, ...) { sample(letters[1:3], size = n, ...) }
#' p <- sample_patch_insert(mtcars, rdist = rdist, replace = TRUE)
#' head(p(mtcars))
#'
sample_patch_insert <- function(df, rdist = stats::rnorm,
                                colname = "INSERTED", exclude_cols = integer(0),
                                seed, ...) {

  if (!missing(seed))
    set.seed(seed)

  candidate_insertion_points <- setdiff(0:ncol(df), exclude_cols - 1L)

  if (length(candidate_insertion_points) == 0)
    stop("All candidate insertion points are excluded.")

  # old: insertion_point <- sample(0:ncol(df), size = 1)
  insertion_point <- sample(candidate_insertion_points, size = 1)
  data <- data.frame(rdist(n = nrow(df), ...))
  colnames(data) <- colname
  patch_insert(insertion_point, data = data)
}
