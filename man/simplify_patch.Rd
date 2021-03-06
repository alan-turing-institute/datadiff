% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patch.R
\name{simplify_patch}
\alias{simplify_patch}
\title{Simplify a composed patch by discarding any superfluous identity patches}
\usage{
simplify_patch(patch)
}
\arguments{
\item{patch}{A \code{patch} object.}
}
\value{
A simplified \code{patch} object.
}
\description{
Simplify a composed patch by discarding any superfluous identity patches
}
\examples{
patch <- compose_patch(patch_identity(),
                       patch_permute(8:1),
                       patch_identity(),
                       patch_shift(2L, shift = 10))
simplify_patch(patch)

}
