#' Test for a patch object
#'
#' Returns \code{TRUE} if the \code{obj} argument is a \code{patch} object.
#'
#' @param obj
#' Any object.
#' @param allow_composed
#' A logicial flag. If \code{TRUE} (the default) then compositions of patches
#' (constructed using either the \code{compose_patch} function or the
#' \code{compose} function from the \code{purrr} package) are considered to be
#' patches. Otherwise, only elementary patch objects are considered to be
#' patches.
#'
#' @export
is_patch <- function(obj, allow_composed = TRUE) {

  if (!methods::is(obj, "patch") && !allow_composed)
    return(FALSE)
  if (!is.function(obj))
    return(FALSE)
  env <- environment(obj)

  # If the obj environment does not contain an object named "fs" then the obj is
  # deemed to be an elementary patch.
  if (!("fs" %in% names(env)))
    return(TRUE)
  if (!allow_composed)
    return(FALSE)

  # If the obj environment contains a list named "fs", all elements of which
  # are themselves (possibly composed) patches, then the obj is deemed to be a
  # composed patch.
  all(purrr::map_lgl(get("fs", envir=env), .f=is_patch, allow_composed=TRUE))
}

#' Test for an identity patch object
#'
#' Returns \code{TRUE} if the \code{obj} argument is an identity patch or, if
#' \code{allow_composed} is \code{TRUE}, a composition of identity patches.
#'
#' @param obj
#' Any object.
#' @param allow_composed
#' A logicial flag. If \code{TRUE} (the default) then compositions of identity
#' patches (constructed using either the \code{compose_patch} function or the
#' \code{compose} function from the \code{purrr} package) are considered to be
#' identity patches. Otherwise, only an elementary identity patch object is
#' considered to be an identity patch.
#'
#' @export
is_identity_patch <- function(obj, allow_composed = TRUE) {
  if (is_patch(obj, allow_composed = FALSE) || !allow_composed)
    return(methods::is(obj, "patch_identity"))
  if (!is_patch(obj, allow_composed = TRUE))
    return(FALSE)
  all(purrr::map_lgl(decompose_patch(obj), .f = is_identity_patch))
}

#' Compose patches
#'
#' Construct multiple patches. The advantage of using this function,
#' rather than calling \code{purrr::compose} directly, is that the class of the
#' return value is \code{patch}, which helps when printing. However both methods
#' will produce objects for which \code{is_patch} returns \code{TRUE}.
#'
#' @param ...
#' n patches (possibly themselves composed) to apply in order from right to left.
#'
#' @return A composite patch object.
#'
#' @seealso \code{\link{compose}}
#'
#' @export
compose_patch <- function(...) {

  ret <- purrr::compose(...)
  # Note: ret is _not_ a patch (yet).
  stopifnot(all(purrr::map_lgl(decompose_patch(ret), is_patch)))
  class(ret) <- c("patch", "function")
  ret
}

#' Decompose a composition of patches
#'
#' Patches may be composed using the \code{\link{compose}} function and the
#' result is a composite patch object. Given such an object, this function
#' returns its constituent elementary \code{patch} objects in a list in the
#' order in which they are applied in the composition. Compositions of
#' compositions are handled via tree recursion.
#'
#' @param patch
#' A composition of \code{patch} objects.
#'
#' @return A list of elementary \code{patch} objects.
#'
#' @export
decompose_patch <- function(patch) {
  stopifnot(is_patch(patch, allow_composed = TRUE))
  if (is_patch(patch, allow_composed = FALSE))
    return(patch)
  unlist(purrr::map(rev(get("fs", envir = environment(patch))), decompose_patch))
}

#' Simplify a composed patch by discarding any superfluous identity patches
#'
#' @param patch
#' A \code{patch} object.
#'
#' @return A simplified \code{patch} object.
#'
#' @export
simplify_patch <- function(patch) {
  stopifnot(is_patch(patch, allow_composed = TRUE))
  if (is_patch(patch, allow_composed = FALSE))
    return(patch)
  if (is_identity_patch(patch, allow_composed = TRUE))
    return(patch_identity())
  patch_list <- purrr::discard(decompose_patch(patch), is_identity_patch,
                               allow_composed = FALSE)
  Reduce(compose_patch, rev(patch_list))
}

#' Get the parameters associated with a patch object.
#'
#' Returns the \code{patch} parameters object.
#'
#' @param patch
#' A \code{patch} object.
#'
#' @return A list containing the parameters associated with the given
#' \code{patch}. If \code{patch} is a composition of patches then a list of
#' lists is returned.
#'
#' @export
get_patch_params <- function(patch) {
  stopifnot(is_patch(patch, allow_composed = TRUE) && is.function(patch))
  if (!is_patch(patch, allow_composed = FALSE))
    return(purrr::map(decompose_patch(patch), get_patch_params))
  env <- environment(patch)
  objs <- mget(names(env), env)
  purrr::discard(objs, is_patch)
}

#### TODO: BUGFIX REQD: test print_patch_params with single column data frame
#parameter


# Convert a patch parameter to a string
#
# @param patch
# A patch object.
# @param param_name
# A parameter name.
# @param ...
# Optional arguments.
#
param_string <- function(patch, param_name, ...) UseMethod("param_string")

# Default implementation of \code{param_string}
#
# @param patch
# A patch object.
# @param param_name
# A parameter name.
# @param digits
# The number of decimal places to be used when printing
# parameters of type \code{double}. Defaults to 3.
# @param ...
# Additional arguments are ignored.
#
param_string.patch <- function(patch, param_name, digits = digits, ...) {

  params <- get_patch_params(patch)
  if (!(param_name %in% names(params)))
    return(character(1))

  x <- params[[param_name]]
  if (is.vector(x) && length(names(x)) == length(x))
    return(paste(names(x), "->", x, collapse = ", "))
  if (is.integer(x) && length(x) <= 30) # TODO.
    return(paste(x, collapse = " "))
  if (is.double(x) && length(x) == 1)
    return(round(x, digits))
  if (!is.list(x) && length(x) == 1)
    return(as.character(x))
  paste0("<", paste(class(x), collapse = "|"), ">")
}

# Permute patch implementation of \code{param_string}
#
# @param patch
# A \code{patch_permute} object.
# @param param_name
# A parameter name.
# @param ...
# Additional arguments passed to the next method.
#
# @return A character string describing the parameters associated with the
# given \code{patch_permute}.
#
param_string.patch_perm <- function(patch, param_name, ...) {

  if (param_name == "perm") {
    params <- get_patch_params(patch)
    stopifnot(param_name %in% names(params))
    perm <- get_patch_params(patch)[[param_name]]
    ret <- paste0(paste(1:length(perm), collapse = " "), "\n",
                  paste(rep(" ", nchar("perm") + 2), collapse = ""),
                  paste(order(perm), collapse = " "))
    return(ret)
  }
  NextMethod(patch, param_name, ...)
}

#' Print the parameters associated with a patch object.
#'
#' @param patch
#' A \code{patch} object.
#' @param digits
#' The number of decimal places to be used when printing
#' parameters of type \code{double}. Defaults to 3.
#'
#' @return A character string describing the parameters associated with the
#' given \code{patch}. If \code{patch} is a composition, the return value is a
#' vector with one element for each elementary patch obtained by calling
#' \code{\link{decompose_patch}} on the \code{patch}.
#'
#' @export
print_patch_params <- function(patch, digits=3) {

  # Handle compositions (will return a character vector).
  if (is_patch(patch, allow_composed = TRUE) &&
      !is_patch(patch, allow_composed = FALSE))
    return(purrr::map_chr(decompose_patch(patch), print_patch_params, digits))

  paste(purrr::map_chr(names(get_patch_params(patch)), .f = function(name) {
    paste(name, param_string(patch, name, digits = digits), sep = ": ")
  }), collapse = "; ")
}

#' Apply a patch to a data frame.
#'
#' Transform one data frame into another by applying a patch.
#'
#' @param df
#' A data frame.
#' @param patch
#' A patch object.
#' @param ...
#' Additional arguments passed to the patch function.
#'
#' @return A transformed data frame.
#'
#' @export
apply_patch <- function(df, patch, ...) {
  stopifnot(is_patch(patch) && is.function(patch))
  ret <- do.call(patch, args = list(df, ...))
  stopifnot(is.data.frame(ret))
  ret
}

#' Get the patch type.
#'
#' Transform one data frame into another by applying a patch.
#'
#' @param patch
#' A patch object.
#' @param short
#' A logical flag. If \code{TRUE} (the default) the prefix "patch_" is omitted
#' from the return value.
#' @return A character string, or a vector if \code{patch} is a composition of
#' patches.
#'
#' @export
patch_type <- function(patch, short=TRUE) {
  stopifnot(is_patch(patch, allow_composed = TRUE))

  if (!is_patch(patch, allow_composed = FALSE))
    return(purrr::map_chr(decompose_patch(patch), .f = patch_type, short = short))
  ret <- setdiff(class(patch), c("patch", "function"))
  if (!short)
    return(ret)
  pattern <- "patch_"
  i <- which(stringr::str_detect(ret, pattern))
  if (length(i) != 1)
    stop("Failed to identify patch type.")
  stringr::str_replace(ret[i], pattern, replacement = "")
}


#' Print a patch object.
#'
#' \code{print} prints its argument and returns it invisibly (via
#' \code{invisible(x)}).
#'
#' To print a composition of patches constructed using the \code{\link{compose}}
#' use \code{\link{decompose_patch}}.
#'
#' @param x
#' A patch object
#' @param ...
#' Any additional arguments are ignored.
#'
#' @seealso \code{\link{decompose_patch}}
#' @export
print.patch <- function(x, ...) {
  if (!is_patch(x, allow_composed = FALSE)) {
    cat("Composed patch with elementary constituents:\n")
    return(invisible(purrr::map(decompose_patch(x), .f = function(p) {
      print(p)
    })))
  }
  type <- paste(setdiff(class(x), c("patch", "function")), sep = ",")
  printed_params <- print_patch_params(x)
  if (length(printed_params) == 1 && nchar(printed_params) == 0)
    cat(type)
  else {
    cat(type, "with parameters:\n")
    cat(paste(print_patch_params(x), collapse = "\n"))
  }
  cat("\n")
  invisible(x)
}
