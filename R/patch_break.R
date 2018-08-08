#' \code{patch_break} S3 class constructor
#'
#' @description
#' S3 class \code{patch_break} which extends the \code{patch} and
#' \code{function} classes to represent a transformation of a tabular dataset
#' by an abrubt break in one or more columns.
#'
#' The broken columns will take their names from the given \code{data}. If
#' \code{colnames(data)} is \code{NULL}, the broken columns will take their
#' names from the corresponding columns in the data frame to which the break
#' patch is applied (i.e. all original column names will be preserved after the
#' break).
#'
#' Break patches are unique in that the mismatch between a broken column and
#' any other column is always taken to be zero.
#'
#' @param cols
#' A vector of column identifiers.
#' @param data
#' A data frame containing the new column data. The number of columns in
#' \code{data} must be equal to the length of \code{cols}.
#'
#' @return A \code{patch_break} object.
#'
#' @export
#'
#' @seealso \code{\link{is_valid_columns}} \code{\link{is_compatible_columns}}
#'
#' @examples
#' patch_break(1L, data = mtcars[1])
#'
patch_break <- function(cols, data) {

  stopifnot(is.data.frame(data))
  stopifnot(ncol(data) == length(cols))

  # Construct the patch object as a closure.
  obj <- function(df) {

    # Check for compatability between the data frame and the parameters.
    stopifnot(is_compatible_columns(cols, df))

    # Identify the relevant column indices (in case the identifiers are not
    # already in integer form).
    if (!is.integer(cols))
      cols <- purrr::map_int(cols, .f = function(x) { which(colnames(df) == x) })

    # Transform the data frame according to the parameters.
    df[cols] <- data # This does not affect the column names.
    if (!is.null(colnames(data)))
      colnames(df)[cols] <- colnames(data)

    stopifnot(is.data.frame(df))
    df
  }

  class(obj) <- c("patch_break", "patch", "function")
  obj
}

#' Randomly sample from a distribution of break patches
#'
#' @param df
#' A data frame
#' @param rdist
#' A function for randomly generating the data in the inserted column. Must
#' contain an argument \code{n} for the number of samples. Defaults to the
#' standard Normal distribution function \code{rnorm}.
#' @param colname
#' The broken column name. If \code{NULL} (the default) the corresponding column
#' name in the data frame \code{df} is maintained.
#' @param condition
#' A predicate which may be used to restrict the break column to those
#' containing a particular type of data. Defaults to the function which always
#' returns \code{TRUE} (i.e. no restriction).
#' @param exclude_cols
#' An integer vector of column indices to be excluded from the set of possible
#' target columns for the returned patch.
#' @param seed
#' A random seed.
#' @param ...
#' Additional arguments passed to the \code{rdist} function.
#'
#' @export
#'
#' @examples
#' head(mtcars)
#' p <- sample_patch_break(mtcars, mean = 10, sd = 4)
#' p
#' head(p(mtcars))
#'
#' # Draw the column data from a given distribution and specify a column name:
#' p <- sample_patch_break(mtcars, rdist = rexp, rate = 2, colname = "BROKEN")
#' p
#' head(p(mtcars))
#'
#' # Draw the column data from a discrete distribution:
#' rdist <- function(n, ...) { sample(letters[1:3], size = n, ...) }
#' p <- sample_patch_break(mtcars, rdist = rdist, replace = TRUE)
#' p
#' head(p(mtcars))
#'
sample_patch_break <- function(df, rdist = stats::rnorm, colname = NULL,
                               condition = function(x) { TRUE },
                               exclude_cols = integer(0), seed, ...) {

  if (!missing(seed))
    set.seed(seed)

  cols <- sample_cols(df, condition = condition, exclude_cols = exclude_cols,
                      size = 1)
  data <- data.frame(rdist(n = nrow(df), ...))
  colnames(data) <- colname
  patch_break(cols, data)
}
