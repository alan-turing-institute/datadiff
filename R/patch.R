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
#'
#' @examples
#' is_patch(patch_rescale(1L, shift = 0, scale_factor = 2))
#' is_patch(patch_identity())
#' is_patch(patch_identity)
#' is_patch(mtcars)
#' is_patch(compose_patch(patch_permute(10:1), patch_delete(1L)))
#' is_patch(compose_patch(patch_permute(10:1), patch_delete(1L)),
#'          allow_composed = FALSE)
#'
is_patch <- function(obj, allow_composed = TRUE) {

  # No good, since composed patches are patch objects if made with compose_patch:
  # if (methods::is(obj, "patch"))
  #   return(TRUE)
  # if (!allow_composed)
  #   return(FALSE)

  if (methods::is(obj, "patch") && allow_composed)
    return(TRUE)
  if (!methods::is(obj, "patch") && !allow_composed)
    return(FALSE)

  # Note that the other two possible cases are trickier:
  # 1. !is patch class & allow composed
  #     - patches composed using purrr::compose rather than compose_patch are
  #       not patch objects by class, but must be considered composed patches.
  # 2. is patch class & !allow composed
  #     - patches composed using compose_patch are patch objects by class, but
  #       must return FALSE if allow composed is FALSE. The hard part is
  #       distinguishing between composed and elementary in this case.
  # To determine these cases we must look at the obj environment.

  if (!is.function(obj))
    return(FALSE)

  composed_fns <- .get_composed_fns(obj)

  # If all functions found in the obj environment (including lists of functions,
  # but *not* including the function named "composed" which is defined in
  # purrr::compose) are themselves (possibly composed) patches, then the obj is
  # deemed to be a composed patch.
  if (!methods::is(obj, "patch") && allow_composed) {
    are_patches <- purrr::map_lgl(composed_fns, .f=is_patch, allow_composed=TRUE)
    return(length(are_patches) != 0 && all(are_patches))
  }

  # The only case left is that in which obj is a patch class object but
  # allow_composed is FALSE, so we just check there is no composition.
  length(composed_fns) == 0
}

# Retrieve composed functions from an environment
#
# Returns a list of the functions (which may themselves be composed) in
# \code{obj} that were composed using purrr::compose, in the order in which
# they are applied in the composition, or an empty list if \code{obj} is *not*
# a composed function.
#
.get_composed_fns <- function(obj) {

  env <- environment(obj)

  # Older versions of purrr store composed functions in a list named "fs"
  if (exists("fs", envir = env))
    return(get("fs", envir = env))

  # Newer versions of purrr store composed functions as a "first_fn" and
  # a list named "fns"
  if(!(exists("first_fn", envir = env) && exists("fns", envir = env)))
    return(list())

  c(list(get("first_fn", envir = env)), get("fns", envir = env))
}

#' Test for an identity patch object
#'
#' Returns \code{TRUE} if the \code{obj} argument is the identity patch or, if
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
#'
#' @examples
#' is_identity_patch(patch_identity())
#' is_identity_patch(compose_patch(patch_identity(), patch_identity()))
#' is_identity_patch(compose_patch(patch_identity(), patch_identity()),
#'                   allow_composed = FALSE)
#' # Patches of other types are not considered to be the identity,
#' # regardless of their parameter values.
#' is_identity_patch(patch_rescale(1L, shift = 0, scale_factor = 1))
#' is_identity_patch(patch_permute(1:10))
#' is_identity_patch(mtcars)
#'
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
#' @seealso \code{\link{decompose_patch}} \code{\link{compose}}
#'
#' @export
#'
#' @examples
#' # Patches are applied from right to left (as in mathematical function
#' # composition) and are printed in order of application:
#' compose_patch(patch_permute(8:1), patch_shift(2L, shift = 10))
#'
#' # Use do.call if the components are in a list.
#' patch_list <- list(patch_permute(8:1), patch_shift(2L, shift = 10))
#' do.call(compose_patch, args = patch_list)
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
#' @seealso \code{\link{compose_patch}}
#'
#' @export
#'
#' @examples
#' patch <- compose_patch(patch_permute(8:1), patch_shift(2L, shift = 10))
#' decompose_patch(patch)
#'
decompose_patch <- function(patch) {
  stopifnot(is_patch(patch, allow_composed = TRUE))
  if (is_patch(patch, allow_composed = FALSE))
    return(list(patch))
  unlist(purrr::map(.get_composed_fns(patch), decompose_patch))
}

#' Simplify a composed patch by discarding any superfluous identity patches
#'
#' @param patch
#' A \code{patch} object.
#'
#' @return A simplified \code{patch} object.
#'
#' @export
#'
#' @examples
#' patch <- compose_patch(patch_identity(),
#'                        patch_permute(8:1),
#'                        patch_identity(),
#'                        patch_shift(2L, shift = 10))
#' simplify_patch(patch)
#'
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
#'
#' @examples
#' get_patch_params(patch_shift(2L, shift = 10))
#' get_patch_params(patch_rescale(1L, shift = 0, scale_factor = 2))
#' get_patch_params(patch_identity())
#'
get_patch_params <- function(patch) {
  stopifnot(is_patch(patch, allow_composed = TRUE) && is.function(patch))
  if (!is_patch(patch, allow_composed = FALSE))
    return(purrr::map(decompose_patch(patch), get_patch_params))
  env <- environment(patch)
  objs <- mget(names(env), env)
  purrr::discard(objs, is_patch)
}

# Convert a patch parameter to a string for printing
#
# @param patch
# A patch object.
# @param param_name
# A parameter name.
# @param df
# (Optional) A data frame compatible with the given \code{patch}.
# @param ...
# Optional arguments.
#
param_string <- function(patch, param_name, df = NULL, ...) UseMethod("param_string")

# Default implementation of \code{param_string}
#
# @param patch
# A patch object.
# @param param_name
# A parameter name.
# @param df
# (Optional) A data frame compatible with the given \code{patch}.
# @param indent
# A character string specifying the intra-patch indentation.
# @param digits
# The number of decimal places to be used when printing
# parameters of type \code{double}. Defaults to 3.
# @param n_int
# The maximum length at which integer vectors are explicitly printed. Defaults
# to 20.
# @param ...
# Additional arguments are ignored.
#
param_string.patch <- function(patch, param_name, df = NULL,
                               indent, digits = 3, n_int = 20, ...) {

  params <- get_patch_params(patch)
  if (!(param_name %in% names(params)))
    return(character(1))

  x <- params[[param_name]]

  if (!is.null(df) && length(colnames(df)) == ncol(df)
      && param_name == "cols" && is.integer(x))
    x <- colnames(df)[x]

  if (is.character(x) && length(names(x)) == 0)
    return(paste(x, collapse = ", "))
  if (is.vector(x) && length(names(x)) == length(x))
    return(paste(names(x), "->", x, collapse = ", "))
  if (is.integer(x) && length(x) <= n_int)
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
# @param df
# (Optional) A data frame compatible with the given \code{patch}.
# @param indent
# A character string specifying the intra-patch indentation.
# @param ...
# Additional arguments passed to the next method.
#
# @return A character string describing the parameters associated with the
# given \code{patchute}.
#
param_string.patch_permute <- function(patch, param_name, df = NULL,
                                       indent, all_cols = FALSE, ...) {

  if (param_name == "perm") {
    params <- get_patch_params(patch)
    stopifnot(param_name %in% names(params))
    perm <- get_patch_params(patch)[[param_name]]

    if (all_cols)
      cols <- 1:length(perm)
    else
      cols <- which(perm != 1:length(perm))

    f_colnames <- identity
    if (!is.null(df) && length(colnames(df)) == ncol(df) && is.integer(cols)) {
      stopifnot(is_compatible_columns(perm, df))
      f_colnames <- f_colnames <- function(x) { colnames(df)[x] }
    }

    ret <- paste0(purrr::map_chr(cols, .f = function(i) {
      paste0(indent, f_colnames((1:length(perm))[i]), " -> ", f_colnames(order(perm)[i]))
    }), collapse = "\n")
    return(ret)
  }
  NextMethod(patch, param_name, df, ...)
}

#' Print the parameters associated with a patch object
#'
#' @param patch
#' An elementary \code{patch} object.
#' @param df
#' (Optional) A data frame compatible with the given \code{patch}.
#' @param indent
#' A character string specifying the intra-patch indentation.
#' @param digits
#' The number of decimal places to be used when printing
#' parameters of type \code{double}. Defaults to 3.
#' @param n_int
#' The maximum length at which integer vectors are explicitly printed. Defaults
#' to 20.
#' @param ...
#' Additional arguments passed to the \code{param_string} function.
#'
#' @return A character string describing the parameters associated with the
#' given \code{patch}. If \code{patch} is a composition, the return value is a
#' vector with one element for each elementary patch obtained by calling
#' \code{\link{decompose_patch}} on the \code{patch}.
#'
#' @export
print_patch_params <- function(patch, df = NULL, indent = "  ", digits = 3,
                               n_int = 20, ...) {

  stopifnot(is_patch(patch, allow_composed = FALSE))

  paste(purrr::map_chr(names(get_patch_params(patch)), .f = function(name) {
    str <- param_string(patch, name, df = df, indent = indent, digits = digits,
                        n_int = n_int, ...)
    if (name == "perm")
      return(str)
    paste(paste0(indent, name), str, sep = ": ")
  }), collapse = "\n")
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
#'
#' @examples
#' patch <- patch_delete(1L)
#' head(mtcars)
#' head(apply_patch(mtcars, patch))
#' # Equivalent to patch function application:
#' head(patch(mtcars))
#'
apply_patch <- function(df, patch, ...) {
  stopifnot(is_patch(patch) && is.function(patch))
  ret <- do.call(patch, args = list(df, ...))
  stopifnot(is.data.frame(ret))
  ret
}

#' Get the patch type.
#'
#' @param patch
#' A patch object.
#' @param short
#' A logical flag. If \code{TRUE} (the default) the prefix "patch_" is omitted
#' from the return value.
#' @param unique
#' A logical flag. If \code{TRUE} any duplicates in the return value will be
#' removed. This may happen only if \code{patch} is a composition. Defaults to
#' \code{FALSE}.
#'
#' @return A character string, or a vector if \code{patch} is a composition of
#' patches.
#'
#' @export
#'
#' @examples
#' patch <- compose_patch(patch_shift(6L, shift = 4),
#'                        patch_permute(8:1),
#'                        patch_shift(2L, shift = 10))
#' patch_type(patch)
#' patch_type(patch, unique = TRUE)
patch_type <- function(patch, short = TRUE, unique = FALSE) {
  stopifnot(is_patch(patch, allow_composed = TRUE))

  if (!is_patch(patch, allow_composed = FALSE)) {
    g <- ifelse(unique, yes = get("unique", pos = baseenv()), no = identity)
    return(g(purrr::map_chr(decompose_patch(patch), .f = patch_type, short = short)))
  }
  ret <- setdiff(class(patch), c("patch", "function"))
  if (!short)
    return(ret)
  pattern <- "patch_"
  i <- which(stringr::str_detect(ret, pattern))
  if (length(i) != 1)
    stop("Failed to identify patch type.")
  stringr::str_replace(ret[i], pattern, replacement = "")
}

#' Admit new column indices
#'
#' Modifies the index parameter in the given \code{patch} to admit the existence
#' of new columns at the positions specified by the \code{cols} argument.
#'
#' @return This function is invoked for its side effect.
#'
#' @param patch
#' An elementary patch object.
#' @param cols
#' A vector of integer column identifiers.
#' @param cols_param_name
#' The name of the relevant column index parameter in the \code{patch}.
#'
#' @export
#'
#' @examples
#' patch <- patch_permute(6:1)
#' patch
#' admit_columns(patch, cols = 3:4)
#' patch
#'
admit_columns <- function(patch, cols, cols_param_name) {

  stopifnot(is.integer(cols))
  stopifnot(is_patch(patch, allow_composed = FALSE))
  if (is_identity_patch(patch))
    return(invisible())
  UseMethod("admit_columns")
}

#' Default implementation of \code{admit_columns} for column-wise patches.
#'
#' @param patch
#' An elementary patch object.
#' @param cols
#' A vector of integer column identifiers.
#' @param cols_param_name
#' The name of the relevant column index parameter in the \code{patch}.
#'
#' @return This function is invoked for its side effect. It returns \code{NULL}
#' invisibly.
#'
#' @keywords internal
#' @export
admit_columns.patch <- function(patch, cols, cols_param_name = "cols") {

  stopifnot(cols_param_name %in% names(get_patch_params(patch)))
  stopifnot(is.integer(get_patch_params(patch)[[cols_param_name]]))

  cols <- sort(cols)
  cols_param <- get_patch_params(patch)[[cols_param_name]]

  # Admit the smallest new column index and call recursively if necessary.
  cols_shift <- as.integer(cols_param >= cols[1])
  assign(cols_param_name, value = cols_param + cols_shift,
         envir = environment(patch), inherits = FALSE)

  if (length(cols) > 1)
    admit_columns(patch, cols = cols[-1], cols_param_name = cols_param_name)
}

#' Insert patch implementation of \code{admit_columns}
#'
#' @param patch
#' An elementary patch object.
#' @param cols
#' A vector of integer column identifiers which are compatible with the
#' permutation in the given \code{patch}.
#' @param cols_param_name
#' The name of the relevant column index parameter in the \code{patch}.
#'
#' @return This function is invoked for its side effect. The return value is
#' that of a tail call to the \code{\link{assign}} function, which is the
#' value assigned to the parameter named in the \code{cols_param_name} argument.
#'
#' @keywords internal
#' @export
admit_columns.patch_insert <- function(patch, cols,
                                       cols_param_name = "insertion_point") {

  stopifnot(cols_param_name %in% names(get_patch_params(patch)))
  stopifnot(is.integer(get_patch_params(patch)[[cols_param_name]]))

  cols <- sort(cols)
  cols_param <- get_patch_params(patch)[[cols_param_name]]

  # Admit the smallest new column index and call recursively if necessary.
  cols_shift <- as.integer(cols_param >= cols[1] - 1)
  assign(cols_param_name, value = cols_param + cols_shift,
         envir = environment(patch), inherits = FALSE)

  if (length(cols) > 1)
    admit_columns(patch, cols = cols[-1], cols_param_name = cols_param_name)
}

#' Permute patch implementation of \code{admit_columns}
#'
#' @param patch
#' An elementary patch object.
#' @param cols
#' A vector of integer column identifiers which are compatible with the
#' permutation in the given \code{patch}.
#' @param cols_param_name
#' The name of the relevant column index parameter in the \code{patch}.
#'
#' @return This function is invoked for its side effect. The return value is
#' that of a tail call to the \code{\link{assign}} function, which is the
#' value assigned to the parameter named in the \code{cols_param_name} argument.
#'
#' @keywords internal
#' @export
admit_columns.patch_permute <- function(patch, cols, cols_param_name = "perm") {

  stopifnot(cols_param_name %in% names(get_patch_params(patch)))
  stopifnot(is.integer(get_patch_params(patch)[[cols_param_name]]))

  cols_param <- get_patch_params(patch)[[cols_param_name]]
  if (!all(cols %in% 1:(length(cols_param) + length(cols))))
    stop("Inadmissible column or columns")

  cols <- sort(cols)
  for (i in seq_along(cols)) {
    i_col <- cols[i]
    cols_param[cols_param >= i_col] = cols_param[cols_param >= i_col] + 1
    pre <- cols_param[1:(i_col - 1)]
    if (i_col == 1)
      pre <- c()
    post <- cols_param[i_col:length(cols_param)]
    if (i_col == length(cols_param) + 1)
      post <- c()
    cols_param <- as.integer(c(pre, i_col, post))
  }
  assign(cols_param_name, value = cols_param, envir = environment(patch),
         inherits = FALSE)
}

#' Identity patch implementation of \code{admit_columns}
#'
#' @param patch
#' An elementary patch object.
#' @param cols
#' A vector of integer column identifiers.
#' @param cols_param_name
#' The name of the relevant column index parameter in the \code{patch}.
#'
#' @return This function is invoked for its side effect, which is no effect
#' whatsoever in this case.
#'
#' @export
admit_columns.patch_identity <- function(patch, cols, cols_param_name) {
}

# Capitalise the first character of a string.
first_toupper <- function(str) {
  paste0(toupper(substring(str, 1, 1)), substring(str, 2, nchar(str)))
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
#' @param df
#' (Optional) A data frame compatible with the given patch \code{x}.
#' @param flag_composed
#' A logical flag for internal use.
#' @param ...
#' Any additional arguments are passed to the \code{print_patch_params} function.
#'
#' @seealso \code{\link{decompose_patch}}
#'
#' @keywords internal
#' @export
print.patch <- function(x, df = NULL, flag_composed = TRUE, ...) {
  if (!is_patch(x, allow_composed = FALSE)) {
    if (flag_composed)
      cat("Composed patch with elementary constituents:\n")
    decomposed <- decompose_patch(x)
    # Print the first elementary patch.
    first_patch <- decomposed[[1]]
    print(first_patch, df = df, ...)
    # If the data frame is given, apply the first patch to it before the
    # recursive call to print (so that structural changes are propagated).
    if (!is.null(df))
      df <- first_patch(df)
    # Recursively print the rest of the patch.
    leftover_patch <- Reduce(compose_patch, rev(decomposed[2:length(decomposed)]))
    return(print(leftover_patch, df, flag_composed = FALSE, ...))
  }
  type <- patch_type(x, short = TRUE)
  printed_params <- print_patch_params(x, df, ...)
  if (length(printed_params) == 1 && nchar(printed_params) == 0)
    cat(type)
  else {
    cat(first_toupper(type), "patch.\n")
    cat(paste(printed_params, collapse = "\n"))
  }
  cat("\n")
  invisible(x)
}
