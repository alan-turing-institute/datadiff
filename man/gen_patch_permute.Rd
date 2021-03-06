% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_patch_permute.R
\name{gen_patch_permute}
\alias{gen_patch_permute}
\title{Generate an optimal permutation patch}
\usage{
gen_patch_permute(df1, df2, mismatch = diffness, ...)
}
\arguments{
\item{df1}{A data frame.}

\item{df2}{A data frame.}

\item{mismatch}{Mismatch method. The default is \code{\link{diffness}}.}

\item{...}{Additional arguments are ignored.}
}
\value{
A \code{patch_permute} object.
}
\description{
Computes the permutation of columns, expressed as a \code{patch_permute} object,
that minimises the mismatch between the data frame \code{df2} and the patched
\code{df1}.
}
\details{
This function uses an assignment problem solver from the package
\code{lpSolve}.
}
\examples{
gen_patch_permute(mtcars, mtcars[, 11:1])

}
