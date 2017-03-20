#' Built-in patch types and predicates
#'
#' @description
#' A collection of standard \code{patch} types and corresponding predicates
#' available at the package level (i.e. in the package namespace environment,
#' accessible via the \code{\link{:::}} operator). Each predicate is a function
#' taking a \code{params} object and returning \code{TRUE} if the form of that
#' object matches that required to define a patch of the corresponding type.
#'
#' For instance, a patch of type \code{delete_patch} represents a transformation
#' which deletes one or more columns from a data frame. The corresponding
#' predicate tests whether the \code{params} object consists of a vector of
#' unique negative integers, returning \code{TRUE} in that case and \code{FALSE}
#' otherwise.
#'
#' @details
#' The \code{datadiff:::type_predicates} list provides a mapping between
#' built-in patch type names and corresponding predicates. This includes the
#' following patch types:
#' \itemize{
#'  \item TYPE: \code{delete_patch}
#'
#'  ACTION: delete one or more columns from a data frame.
#'
#'  PREDICATE: \code{params} must be a non-empty vector of unique negative
#'  integers.
#'
#'  \item TYPE: \code{permute_patch}
#'
#'  ACTION: permute the columns of a data frame.
#'
#'  PREDICATE: \code{params} must be a vector of unique positive integers of
#'  length two or more.
#'
#'  \item TYPE: \code{encode_patch}
#'
#'  ACTION: re-encode categorical data in specified columns.
#'
#'  PREDICATE: \code{params} must be a list with two elements whose names match
#'  the character strings contained in \code{datadiff:::COLUMNS} and
#'  \code{datadiff:::ENCODING}.
#'  \itemize{
#'   \item The element named \code{datadiff:::COLUMNS} must be a non-empty
#'   vector of unique positive integers.
#'   \item The element named \code{datadiff:::ENCODING} must be a non-empty
#'   factor containing unique elements with unique names.
#'   }
#'
#'  \item TYPE: \code{scale_patch}
#'
#'  ACTION: rescale numerical data in specified columns.
#'
#'  PREDICATE: \code{params} must be a list with two elements whose names match
#'  the character strings contained in \code{datadiff:::COLUMNS} and
#'  \code{datadiff:::SCALE_FACTOR}.
#'  \itemize{
#'   \item The element named \code{datadiff:::COLUMNS} must be a non-empty
#'   vector of unique positive integers.
#'   \item The element named \code{datadiff:::SCALE_FACTOR} must be a numerical
#'   scalar value.
#'   }
#' }
#'
#' @seealso \code{\link{patch}}
#' @name predicates
NULL
TYPE_DELETE <- "delete_patch" # Delete one or more columns.
TYPE_PERMUTE <- "permute_patch" # Permute the columns.
TYPE_ENCODE <- "encode_patch" # Re-encode categorical data.
TYPE_SCALE <- "scale_patch" # Rescale numerical data.

# Note that we intentionally do _not_ support column names (i.e. we only accept
# column indices) as names can be easily converted to indices before calling
# the patch (i.e. when the data frame is available) and their inclusion here
# would louse up the type inference (see PREDICATE_DELETE, for instance).
PREDICATE_COLUMNS <- function(params) {
  length(params) != 0 && is.integer(params) && all(params > 0) &&
    !any(duplicated(params)) && !any(is.na(params))
}
PREDICATE_ENCODING <- function(params) {
  length(params) != 0 && is.factor(params) &&
    length(names(params)) == length(params) && !any(duplicated(params)) &&
    !any(duplicated(names(params))) && !any(is.na(params))
}

## Delete patch type.
PREDICATE_DELETE <- function(params) {
  is.integer(params) && PREDICATE_COLUMNS(-params)
}

## Permute patch type.
PREDICATE_PERMUTE <- function(params) {
  PREDICATE_COLUMNS(params) && length(params) > 1
}

## Encode patch type.
ENCODING <- "ENCODING"
COLUMNS <- "COLUMNS"
PREDICATE_ENCODE <- function(params) {
  if (!is.list(params))
    return(FALSE)
  if (length(params) != 2)
    return(FALSE)
  if (!setequal(names(params), c(ENCODING, COLUMNS)))
    return(FALSE)
  PREDICATE_COLUMNS(params[[COLUMNS]]) && PREDICATE_ENCODING(params[[ENCODING]])
}

## Scale patch type.
SCALE_FACTOR <- "SCALE_FACTOR"
PREDICATE_SCALE <- function(params) {
  if (!is.list(params))
    return(FALSE)
  if (length(params) != 2)
    return(FALSE)
  if (!setequal(names(params), c(SCALE_FACTOR, COLUMNS)))
    return(FALSE)
  if (!PREDICATE_COLUMNS(params[[COLUMNS]]))
    return(FALSE)
  length(params[[SCALE_FACTOR]]) == 1 && is.numeric(params[[SCALE_FACTOR]])
}
type_predicates <- list(PREDICATE_DELETE, PREDICATE_PERMUTE, PREDICATE_ENCODE, PREDICATE_SCALE)
names(type_predicates) <- c(TYPE_DELETE, TYPE_PERMUTE, TYPE_ENCODE, TYPE_SCALE)

