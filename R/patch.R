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

#' Compose patches
#'
#' Construct a composite patch object. The advantage of using this function,
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

### TODO NEXT: DECIDE HOW TO IDENTIFY ELEMENTARY PATCHES IN is_patch.


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
  stopifnot(is_patch(patch))
  if (is_patch(patch, allow_composed = FALSE))
    return(patch)
  unlist(purrr::map(rev(get("fs", envir = environment(patch))), decompose_patch))
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

  params <- get_patch_params(patch)
  if (length(params) == 0)
    return(character(1))
  param_string <- function(x) {
    if (is.integer(x) && length(x) <= 10) # TODO.
      return(paste(x, collapse = " "))
    if (is.double(x) && length(x) == 1)
      return(round(x, digits))
    if (!is.list(x) && length(x) == 1)
      return(as.character(x))
    paste0("<", paste(class(x), collapse = "|"), ">")
  }
  paste(purrr::map_chr(1:length(params), .f = function(i) {
    paste(names(params)[i], param_string(params[[i]]), sep = ": ")
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
