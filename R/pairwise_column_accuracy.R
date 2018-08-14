#' Compute pairwise column accuracy
#'
#' Returns the column accuracy of the \code{result} w.r.t. the \code{target}
#' patch as a number in the unit interval.
#'
#' If \code{partial} is FALSE the return value is binary: a one is returned
#' if and only if \eqn{A}, the set of columns transformed by the \code{result},
#' is identical to \eqn{B}, the set of columns transformed by the \code{target}
#' patch. If \code{partial} is \code{TRUE}, partial credit is given whenever the
#' two sets of columns share a non-empty intersection. In this case, the return
#' value is the fraction:
#' \eqn{|A \cap B|/\mbox{max}(|A|, |B|)}
#'
#' Returns \code{NA} if the \code{target} contains elementary component(s) of the
#' given type but the \code{result} does not.
#'
#' An error is thrown if the \code{target} does not contain an elementary component
#' of the given type, or if either the \code{target} or the \code{result} is not
#' column-wise unique for the given patch \code{type}.
#'
#' @param target
#' A patch.
#' @param result
#' A patch.
#' @param type
#' A patch type.
#' @param partial
#' A logical flag. If \code{TRUE}, partial credit is given (see the details),
#' otherwise the return value is binary. Defaults to \code{FALSE}.
#' @param short
#' A logical flag passed to the \code{\link{patch_type}} function. If
#' \code{TRUE} (the default) the \code{type} argument must specify the patch
#' type in short form.
#' @param column_param
#' The name of the column parameter in patches of the specified type.
#'
#' @return A number between 0 and 1, or \code{NA} .
#'
#' @seealso \code{\link{metric_column_accuracy}}
#'
#' @export
#'
#' @examples
#' target <- compose_patch(patch_permute(4:1),
#'                        patch_rescale(4L, shift = 0, scale_factor = 2))
#' # Column-accurate:
#' result <- compose_patch(patch_permute(c(4L, 2L, 3L, 1L)),
#'                         patch_rescale(4L, shift = 10, scale_factor = 20))
#' pairwise_column_accuracy(target, result, type = "rescale")
#'
#' # Not column-accurate:
#' result <- compose_patch(patch_rescale(1L, shift = 10, scale_factor = 20))
#' pairwise_column_accuracy(target, result, type = "rescale")
#'
pairwise_column_accuracy <- function(target, result, type, partial = FALSE, short = TRUE,
                                     column_param = "cols") {

  stopifnot(length(column_param) == 1)
  # Note that this will throw an error if the target does not contain an
  # elementary component of the given type.
  stopifnot(is_columnwise_unique(target, type = type, short = short,
                                 column_param = column_param))

  # If the result does not contain any elementary component of the given type,
  # return NA. This type of failure is captured by the Type Recall metric and
  # must not be confused with failed column accuracy (which specifically refers
  # to the correct identification of patch type but with incorrect column parameter).
  if (!(type %in% patch_type(result, short = short)))
    return(NA)

  stopifnot(is_columnwise_unique(result, type = type, short = short,
                                 column_param = column_param))

  # Function to return the derived column indices associated with a patch. The
  # initial argument is a boolean flag used to select whether derived column
  # indices should be the initial or the final column positions. The latter is
  # required for the insert patch type.
  new_positions <- function(patch, initial) {

    # Identify all elementary components of the given type.
    decomposed <- decompose_patch(patch)
    type_match <- purrr::map_lgl(decomposed, .f = function(p) {
      identical(patch_type(p, short = short), type)
    })

    # Check that all elementary patches matching the given type contain the
    # column index parameter.
    stopifnot(all(purrr::map_lgl(decomposed[type_match], .f = function(p) {
      column_param %in% names(get_patch_params(p))
    })))

    purrr::map_int(which(type_match), .f = function(k) {
      given_position <- get_patch_params(decomposed[[k]])[[column_param]]
      # Note that the given position in an insert patch is the column index after
      # which the new column is inserted, hence given_position + 1.
      if (!initial)
        given_position <- given_position + 1L

      if (!initial && k == length(decomposed))
        return(given_position)

      if (initial)
        residual_indices <- 1:k
      else
        residual_indices <- (k + 1):length(decomposed)

      terminal_column_position(given_position,
                               patch = Reduce(compose_patch, rev(decomposed[residual_indices])),
                               initial = initial)
    })
  }

  initial <- TRUE
  # Set the initial flag to FALSE only for the insert patch type.
  if (identical(type, patch_type(patch_insert(0L, data.frame(NA)), short = short)))
    initial <- FALSE

  target_nps <- new_positions(target, initial)
  result_nps <- new_positions(result, initial)

  if (!partial)
    return(ifelse(setequal(target_nps, result_nps), yes = 1, no = 0))

  length(intersect(target_nps, result_nps))/max(length(target_nps), length(result_nps))
}
