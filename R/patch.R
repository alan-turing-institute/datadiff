#' Patch S3 class constructor.
#'
#' @description
#' S3 class \code{patch} which extends the \code{function} class to represent a
#' generic transformation of a tabular dataset. The transformation is executed
#' by evaluating the \code{patch} (function) on a data frame. Any additional
#' arguments passed to the \code{patch} function are passed on to the
#' \code{is_compatible} and \code{return_value} methods (see the 'Details'
#' section below).
#'
#' See the 'Details' section for a step-by-step guide explaining how to create
#' new patch types to enable dataset transformations which are not already
#' supported.
#'
#' @details
#' A \code{patch} is a closure with an object named \code{params} in its
#' enclosing environment, accessed via the \code{\link{patch_params}} function.
#' The form of the \code{params} object determines the type of the \code{patch}
#' (via a corresponding predicate), which in turn determines the form of the
#' transformation. The \code{\link{patch_type}} function returns the type as a
#' character string. The specific operation of a \code{patch} instance is
#' determined by the value(s) inside its \code{params} object according to the
#' rule encoded in the (generic) function \code{return_value}.
#'
#' For example, if \code{params} is a non-empty vector of unique, negative
#' integers then the corresponding patch type is \code{delete_patch},
#' representing a transformation which deletes one or more columns from a data
#' frame. The (negative) integer values in the vector specify the indices of
#' the columns to be deleted.
#'
#' Several \code{patch} types are supported by default (see
#' \code{\link{predicates}} for details of built-in types). The \code{patch}
#' class has also been designed to allow new types to be included with minimal
#' effort and to thereby support arbitrary transformations of (tabular)
#' datasets.
#'
#' To add a new patch type:
#' \enumerate{
#'  \item Choose a name for the new type which does not conflict with any of
#'  the existing ones - see \code{names(datadiff:::type_predicates)}.
#'
#'  \item Implement the generic methods \code{is_compatible} and
#'  \code{return_value} by writing new functions named
#'  \code{is_compatible.<new_type_name>} and
#'  \code{return_value.<new_type_name>}, respectively. It is recommended that
#'  this is done in a file whose name matches the name of the type.
#'
#'  Simple examples, for the \code{delete_patch} type, are
#'  \code{\link{is_compatible.delete_patch}} and
#'  \code{\link{return_value.delete_patch}}.
#'
#'  \item Construct a predicate to enable the new type to be inferred from the
#'  patch parameters object.
#'
#'  Examples of such predicates may be found in the
#'  \code{datadiff:::type_predicates} list. The new predicate must not conflict
#'  with the existing ones in the sense that, for any given \code{params}
#'  object, at most one predicate may return \code{TRUE}.
#'
#'  \item Construct a named list which contains the new predicate (constructed
#'  in step 3) with corresponding element name precisely matching the new type
#'  name (i.e. identical to the suffix \code{<new_type_name>} used when
#'  implementing the generic methods in step 2).
#'
#'  The elements in the \code{datadiff:::type_predicates} list will be
#'  automatically added to this list. Note that, for any given (valid)
#'  \code{params} object, precisely one predicate must evaluate to \code{TRUE}
#'  otherwise the \code{patch} constructor will fail with an error.
#'
#'  \item Construct \code{patch} objects of the new type by calling the usual
#'  constructor and passing both the \code{params} object and the list of
#'  predicates constructed in step 4.
#' }
#'
#' @param params
#' A parameters object which determines both the patch type and the specific
#' action of the \code{patch}.
#' @param predicates
#' A optional named list of predicates. This should be ignored except when
#' using additional \code{patch} types (i.e. types which are not supported by
#' default). See the 'Details' section below.
#'
#' Each predicate must enable the particular \code{patch} type to be determined
#' (by returning \code{TRUE} when called on a corresponding \code{params}
#' object and \code{FALSE} otherwise). Element names in the \code{predicates}
#' list must precisely match the corresponding \code{patch} type names.
#'
#' The built-in predicates contained in the \code{datadiff:::type_predicates}
#' object will be added to this list (overwriting any existing predicates with
#' matching names). Note that, for any given \code{params} object, precisely
#' one predicate must evaluate to \code{TRUE} otherwise the \code{patch}
#' constructor will fail with an error.
#'
#' @seealso \code{\link{predicates}} \code{\link{patch_type}}
#' \code{\link{patch_params}}
#'
#' @import purrr
#' @export
#'
#' @examples
#'
#' # Delete a column.
#' colnames(mtcars)
#' p1 <- patch(-2L)
#' colnames(p1(mtcars))
#'
#' # Permute some columns.
#' p2 <- patch(c(2L, 5L, 7L))
#' colnames(p2(mtcars))
#'
#' # Re-encode categorical data.
#' purrr::map_chr(mtcars, class)
#' p3 <- patch(encode_patch_params(c(8L, 9L), c("0" = FALSE, "1" = TRUE)))
#' purrr::map_chr(p3(mtcars), class)
#'
#' # Rescale numerical data.
#' mean(mtcars[[1]]) # Average miles per gallon
#' p4 <- patch(scale_patch_params(1L, 0.425144))
#' mean(p4(mtcars)[[1]]) # Average km per litre
#'
#' # Patches may be composed. The following are equivalent:
#' purrr::map_chr(p1(p2(p3(p4(mtcars)))), class)
#' purrr::map_chr(purrr::compose(p1, p2, p3, p4)(mtcars), class)
#' purrr::map_chr(Reduce(purrr::compose, list(p1, p2, p3, p4))(mtcars), class)
#'
#' # Attempting to apply a patch to an incompatible data frame throws an error.
#' \dontrun{
#' p <- patch(-as.integer(ncol(mtcars) + 1))
#' p(mtcars)
#' }
patch <- function(params, predicates = list()) {

  # Construct the patch object as a closure.
  obj <- function(df, ...) {

    # Get the patch object (inside its own body!) from the enclosing envir.
    encl_env <- parent.env(environment())
    is_a_patch <- purrr::map_lgl(mget(names(encl_env), envir = encl_env),
                                 is_patch)

    if (!any(is_a_patch) || sum(is_a_patch) != 1)
      warning("Failed to obtain patch object: compatibility test omitted.")
    p <- get(names(is_a_patch)[is_a_patch], envir = encl_env)

    # Check compatibility with the data frame.
    if (!is_compatible(p, df, ...))
      stop(paste("Incompatible patch & data frame. Patch type:", patch_type(p)))

    # Generate the return value.
    ret <- return_value(p, df, ...)

    if (!is.data.frame(ret))
      stop(paste("Result must be a data frame. Check the return_value method for
                 type", patch_type(p)))

    ret
  }

  # Make sure that the predicates list contains the default types.
  predicates[names(datadiff:::type_predicates)] <- datadiff:::type_predicates

  # Infer the patch type by evaluating the predicates against the parameters.
  matched_type <- purrr::map_lgl(predicates, function(pred) { pred(params) })

  if (sum(matched_type) != 1)
    stop("Failed to infer patch type from the given params and predicates.")

  type <- names(matched_type)[matched_type]
  if (length(type) == 0 || nchar(type) == 0)
    stop("Empty type name. Check that the predicates argument is a named list.")

  class(obj) <- c(type, "patch", "function")
  obj
}

# Create the generic function 'is_compatible'.
#' Test for compatibility between patch and data frame
#'
#' @param obj
#' A \code{patch} object.
#' @param df
#' A data frame.
#' @param ...
#' Additional arguments.
#'
#' @export
is_compatible <- function(obj, df, ...) { UseMethod("is_compatible") }

# Create the generic function 'return_value'.
#' Generate the patch return value.
#'
#' @param obj
#' A \code{patch} object.
#' @param df
#' A data frame.
#' @param ...
#' Additional arguments.
#'
#'@export
return_value <- function(obj, df, ...) { UseMethod("return_value") }

#' Test for a patch object.
#'
#' Returns \code{TRUE} if the \code{obj} argument is a \code{patch} object.
#'
#' @param obj
#' Any object.
#'
#' @importFrom methods is
#' @export
is_patch <- function(obj) {
  is(obj, "patch")
}

#' Get the parameters of a patch object.
#'
#' Returns the \code{patch} parameters object.
#'
#' @param obj
#' A \code{patch} object.
#' @export
patch_params <- function(obj) {
  stopifnot(is_patch(obj))
  get("params", environment(obj))
}

#' Get the type of a patch object.
#'
#' Returns the \code{patch} type as a character string.
#'
#' @param obj
#' A \code{patch} object.
#'
#' @import purrr
#' @export
patch_type <- function(obj) {
  stopifnot(is_patch(obj))
  predicates <- get("predicates", environment(obj))
  params <- get("params", environment(obj))
  index <- purrr::map_lgl(predicates, function(pred) { pred(params) })
  names(predicates)[index]
}

#' Print a patch object.
#'
#' \code{print} prints its argument and returns it invisibly (via
#' \code{invisible(x)}).
#'
#' @param x
#' A patch object
#' @param ...
#' Any additional arguments are ignored.
#'
#' @export
print.patch <- function(x, ...) {
  cat(patch_type(x), "with parameters:\n")
  print(patch_params(x))
  invisible(x)
}
