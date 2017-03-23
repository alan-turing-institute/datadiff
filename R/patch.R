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
  methods::is(obj, "patch")
}

#' Get the parameters associated with a patch object.
#'
#' Returns the \code{patch} parameters object.
#'
#' @param patch
#' A \code{patch} object.
#'
#' @return An object containing the parameters associated with the given
#' \code{patch}
#'
#' @import purrr
#' @export
get_patch_params <- function(patch) {
  stopifnot(is_patch(patch) && is.function(patch))
  objs <- mget(names(environment(patch)), environment(patch))
  purrr::discard(objs, is_patch)
}

#' Apply a patch to a data frame.
#'
#' Transform one data frame into another by applying a patch.
#'
#' @param patch
#' A patch object.
#' @param df
#' A data frame.
#' @param ...
#' Additional arguments passed to the patch function.
#'
#' @return A transformed data frame.
#'
#' @export
apply_patch <- function(patch, df, ...) {
  stopifnot(is_patch(patch) && is.function(patch))
  ret <- do.call(patch, args = list(df, ...))
  stopifnot(is.data.frame(ret))
  ret
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
  type <- paste(setdiff(class(x), c("patch", "function")), sep = ",")
  cat(type, "with parameters:\n")
  print(get_patch_params(x))
  invisible(x)
}

