#' Constraint match
#'
#' @export
constraint_match <- function(col1, col2) {
  r <- list(col1=col1, col2=col2)
  class(r) <- "constraint_match"
  r
}

#' Is constraint match with col1
#'
#' @export
is_constraint_match_col1 <- function(c, col1) {
  methods::is(c, "constraint_match") && c$col1 == col1
}

#' Is constraint match with col2
#'
#' @export
is_constraint_match_col2 <- function(c, col2) {
  methods::is(c, "constraint_match") && c$col2 == col2
}

#' Constraint nomatch
#'
#' @export
constraint_nomatch <- function(col1, col2) {
  r <- list(col1=col1, col2=col2)
  class(r) <- "constraint_nomatch"
  r
}

#' Is constraint nomatch
#'
#' @export
is_constraint_nomatch <- function(c, col1, col2) {
  methods::is(c, "constraint_nomatch") && c$col1 == col1 && c$col2 == col2
}

#' Constraint notransform
#'
#' @export
constraint_notransform <- function(col) {
  r <- list(col=col)
  class(r) <- "constraint_notransform"
  r
}

#' Constraint notransform
#'
#' @export
is_constraint_notransform <- function(c, col) {
  methods::is(c, "constraint_notransform") && c$col == col
}

#' Never patch
#'
#' @export
patch_never <- function() {
  obj <- function(df) {
    stop("This patch should never be used!")
  }
  class(obj) <- c("patch_never", "patch", "function")
  obj
}
