% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batch_experiment.R
\name{batch_experiment}
\alias{batch_experiment}
\title{Configure & execute a batch of synthetic experiments}
\usage{
batch_experiment(data_ids, corruptions, datadiff = ddiff, N = 20,
  split = 0.5, seed = sample.int(.Machine$integer.max, size = 1),
  data_reader = get, execute = TRUE, pb = TRUE, quiet = TRUE,
  logfile = NULL)
}
\arguments{
\item{data_ids}{A collection of data frame identifiers. Each one must be a valid
\code{data_id} input to \code{\link{configure_synthetic_experiment}}.}

\item{corruptions}{A collection of patch sampler functions. Each one must be a valid
\code{corruption} input to \code{\link{configure_synthetic_experiment}}.}

\item{datadiff}{The datadiff function for the experiment.}

\item{N}{The number of experiments (i.e. random splits of the data) in each batch.
Defaults to 20.}

\item{split}{A number in the unit interval specifying the splitting ratio.}

\item{seed}{A random seed. By default an integer seed is chosen at random.}

\item{data_reader}{A function which reads the data, given each of the arguments in \code{data_id}.}

\item{execute}{A logical flag. If \code{TRUE} (the default) the batch experiment will be
executed, and the outputs returned. Otherwise the batch of experiment
configurations will be returned without performing the experiments.}

\item{pb}{A logical flag. If \code{TRUE} (the default) a progress bar will be
displayed in the console.}

\item{quiet}{A logical flag. If \code{TRUE} an error message will be printed to the
console in the event of a failure to execute any particular synthetic
experiment in the batch. Defaults to \code{FALSE}.}

\item{logfile}{(Optional) The full path to a logfile.}
}
\value{
A list of lists of exectuted \code{synthetic_experiment} objects. The
outer list corresponds to the set of datasets specified in the \code{data_ids}
argument. The inner list corresponds to the list of \code{corruptions}.
}
\description{
Use this function to run synthetic experiments sequentially on multiple
datasets and with multiple corruptions. To process the results, pass the
return value to the \code{batch_summary} function.
}
\examples{
\dontrun{
data_ids <- c("mtcars", "airquality")
corruptions <- list(sample_patch_identity,
                   sample_patch_permute,
                   sample_patch_insert,
                   list(sample_patch_delete, sample_patch_permute))
batch_experiment(data_ids, corruptions = corruptions, N = 2, data_reader = get)
}
}
\seealso{
\code{\link{configure_synthetic_experiment}}
\code{\link{execute_synthetic_experiment}}
}
