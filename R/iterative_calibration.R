#' Calibrate penalty parameters by iterative bootstrapping
#'
#' @param data_ids
#' A character vector of data frame identifiers. Each one must be a valid
#' \code{data_id} input to \code{\link{configure_synthetic_experiment}}.
#' @param datadiff
#' The datadiff function for the experiment.
#' @param patch_generators
#' A list of patch generator functions from which candidate patches will be generated.
#' @param patch_penalties
#' A numeric vector of patch penalties corresponding to the \code{patch_generators}
#' list. The lengths of these two arguments must be equal.
#' @param permute_penalty
#' The penalty associated with a permutation patch.
#' @param break_penalty
#' The penalty associated with a break patch.
#' @param N
#' The number of experiments (i.e. random splits of the data) per random seed.
#' @param M
#' The number of random seeds to be sampled from the \code{hyperseed}.
#' @param split
#' A number in the unit interval specifying the splitting ratio.
#' @param hyperseed
#' A random seed used to select the \code{M} seeds for each corruption. By
#' default an integer seed is chosen at random.
#' @param data_reader
#' A function which reads the data, given each of the arguments in \code{data_id}.
#' @param target_fpr
#' The target false positive rate.
#' @param acceptance_margin
#' The acceptable margin around the target false positive rate.
#' @param increment_factor
#' The factor by which penalties are incremented between iterations.
#' @param decrement_factor
#' The factor by which penalties are decremented between iterations.
#' @param lower_boundary
#' The lower boundary which determines the domain for patch penalty values.
#' @param upper_boundary
#' The lower boundary which determines the domain for patch penalty values.
#' @param check_viability
#' A logical flag. If \code{TRUE} (the default) a preliminary viability run will
#' be executed with all penalties set to their lower boundary values. The
#' calibration procedure is valid for a given patch type if the false positive
#' rate for that type is inside or above the acceptance interval when the
#' penalty takes the \code{lower_boundary} value.
#' @param pb
#' A logical flag. If \code{TRUE} (the default) a progress bar will be
#' displayed in the console.
#' @param logfile
#' (Optional) The full path to a logfile.
#'
#' @return A named numeric vector.
#'
#' @export
iterative_calibration <- function(data_ids,
                                  datadiff = ddiff,
                                  patch_generators = list(gen_patch_rescale, gen_patch_recode),
                                  patch_penalties = c(0.4, 0.4),
                                  permute_penalty = 0.1,
                                  break_penalty = 0.95,
                                  N = 10,
                                  M = 10,
                                  split = 0.5,
                                  hyperseed = sample.int(.Machine$integer.max, size = 1),
                                  data_reader = get,
                                  target_fpr = 0.05,
                                  acceptance_margin = 0.1,
                                  increment_factor = 1.2,
                                  decrement_factor = 0.8,
                                  lower_boundary = 10^(-6),
                                  upper_boundary = 1 - lower_boundary,
                                  check_viability = TRUE,
                                  pb = TRUE,
                                  logfile = NULL) {

  stopifnot(is.character(data_ids))
  stopifnot(length(split) == 1)
  stopifnot(target_fpr > 0 && target_fpr < 1)
  stopifnot(acceptance_margin > 0 && acceptance_margin < 1)
  stopifnot(increment_factor > 1)
  stopifnot(decrement_factor < 1)
  stopifnot(decrement_factor > 0)

  # Check that N, M, target_fpr and acceptance_margin are jointly sensible.
  if (1/(N*M) > 2 * acceptance_margin * target_fpr)
    stop("Incompatible parameters. Try increasing N and/or M.")

  if (!is.null(logfile)) {
    logging::addHandler(logging::writeToFile, file=logfile, level='DEBUG')
    logging::loginfo("Iterative calibration with target FPR = %f, margin = %f, hyperseed = %d",
                     target_fpr, acceptance_margin, hyperseed)
  }

  # Identify the relevant patch types by dataset (required for correct
  # computation of the joint false positive rates).
  gen_types_by_data_id <- purrr::map(data_ids, .f = function(data_id) {
    df <- data_reader(data_id)
    purrr::discard(generator_type(df, patch_generator = patch_generators, short = TRUE),
                   .p = is.na)
  })
  names(gen_types_by_data_id) <- data_ids

  # Identify all of the relevant patch types, given the data.
  gen_types <- unique(unlist(gen_types_by_data_id))

  # Drop any incompatible patch types.
  is_compatible_type <- !is.na(gen_types)
  patch_generators <- patch_generators[is_compatible_type]
  gen_types <- gen_types[is_compatible_type]

  candidate_patch_penalties <- patch_penalties[is_compatible_type]
  names(candidate_patch_penalties) <- gen_types[is_compatible_type]

  # Add the permute patch type to get the complete set of relevant types.
  permute_type <- patch_type(patch_permute(1L), short = TRUE)
  types <- c(gen_types[is_compatible_type], permute_type)

  candidate_permute_penalty <- permute_penalty
  names(candidate_permute_penalty) <- patch_type(patch_permute(1L), short = TRUE)

  candidate_penalties <- c(candidate_patch_penalties, candidate_permute_penalty)

  run_experiment <- function(candidate_penalties, seed) {
    experiment_datadiff <- purrr::partial(datadiff,
                                          patch_generators = patch_generators,
                                          patch_penalties = candidate_penalties[gen_types],
                                          permute_penalty = candidate_penalties[permute_type],
                                          break_penalty = break_penalty)

    multi_batch_experiment(data_ids = data_ids,
                           corruptions = list(sample_patch_identity),
                           datadiff = experiment_datadiff,
                           N = N,
                           M = M,
                           split = split,
                           hyperseed = seed,
                           data_reader = get,
                           execute = TRUE,
                           pb = pb,
                           logfile = logfile)
  }

  joint_false_positive_rate <- function(batch_output) {

    # Compute the false positive rates for all datasets and all patch types.
    # Note that it is essential to pass in the correct, dataset-specific type
    # argument here.
    fprs <- purrr::map(data_ids, .f = function(data_id) {
      type <- c(gen_types_by_data_id[[data_id]], permute_type)
      false_positive_rate(batch_output[[data_id]][[1]], type = type, short = TRUE)
    })
    names(fprs) <- data_ids

    # Compute the joint false positive rate for *all* types.
    fpr <- purrr::map_dbl(types, .f = function(type) {
      mean(purrr::map_dbl(data_ids, .f = function(data_id) {
        if (type %in% names(fprs[[data_id]]))
          return(fprs[[data_id]][type])
        as.double(NA)
      }), na.rm = TRUE)
    })
    names(fpr) <- types
    fpr
  }

  is_acceptable_fpr <- function(x) {
    x > (1 - acceptance_margin) * target_fpr &
      x < (1 + acceptance_margin) * target_fpr
  }

  # Before starting the loop, do a single experiment with all parameters set
  # to the boundary value (i.e. the minimum value to which the loop might
  # converge). Then do a sanity check by looking at these maximum possible
  # false positive rates. If any are below (1 - acceptance_margin) * target_fpr
  # then there's no chance of successfully calibrating those parameters.
  if (check_viability) {
    boundary_penalties <- rep(lower_boundary, times = length(candidate_penalties))
    names(boundary_penalties) <- names(candidate_penalties)

    if (!is.null(logfile))
      logging::loginfo(paste("Preliminary run at parameter lower boundary of", lower_boundary))

    boundary_result <- run_experiment(boundary_penalties, seed = hyperseed)

    boundary_fpr <- joint_false_positive_rate(boundary_result)

    if (!is.null(logfile)) {
      logging::loginfo(paste("Lower boundary FPR:",
                             paste(c(rbind(names(boundary_fpr), format(boundary_fpr, digits = 3))),
                                   collapse = " ")))
    }

    # Check for any false positive rates below the acceptable region under the
    # bounday (i.e. minimum) parameter values. Assign the boundary value to any
    # such parameters and record them as "non-viable".
    viable_fpr_at_boundary <- boundary_fpr > (1 - acceptance_margin) * target_fpr
    if (!is.null(logfile)) {
      logging::loginfo(paste("Viability given boundary:",
                             paste(c(rbind(names(viable_fpr_at_boundary),
                                           viable_fpr_at_boundary)), collapse = " ")))
    }

    if (any(!viable_fpr_at_boundary))
      candidate_penalties[!viable_fpr_at_boundary] <- lower_boundary

    if (all(!viable_fpr_at_boundary)) {
      if (!is.null(logfile))
        logging::loginfo("Iterative calibration attempt failed: no viable parameters at lower boundary of %f",
                         lower_boundary)
      return(NA)
    }

    accept_at_boundary <- is_acceptable_fpr(boundary_fpr)
    if (all(accept_at_boundary | !viable_fpr_at_boundary)) {
      msg <- "Iterative calibration succeeded at the lower boundary"
      if (any(!viable_fpr_at_boundary)) {
        msg <- paste(msg, "except for non-viable parameters:",
                     paste(names(candidate_penalties)[!viable_fpr_at_boundary],
                           collapse = ", "))
        candidate_penalties[!viable_fpr_at_boundary] <- NA
      }
      if (!is.null(logfile))
        logging::loginfo(msg)
      return(candidate_penalties)
    }
  } else {
    # If check_viability is FALSE, assume viability for all patch types.
    if (!is.null(logfile))
      logging::loginfo("Skipping preliminary run; viability assumed for all patch types.")
    viable_fpr_at_boundary <- rep(TRUE, times = length(candidate_penalties))
    names(viable_fpr_at_boundary) <- names(candidate_penalties)
  }

  set.seed(hyperseed)
  while(TRUE) {
    if (!is.null(logfile))
      logging::loginfo(paste("Current candidates:",
                             paste(c(rbind(names(candidate_penalties), format(candidate_penalties, digits = 3))),
                                   collapse = " ")))

    # Run a multi-batch experiment with the candidate penalty parameters.
    experiment_hyperseed <- sample.int(.Machine$integer.max, size = 1)
    result <- run_experiment(candidate_penalties, seed = experiment_hyperseed)

    fpr <- joint_false_positive_rate(result)
    accept <- is_acceptable_fpr(fpr)

    if (!is.null(logfile)) {
      logging::loginfo(paste("FPR:       ", paste(c(rbind(names(fpr), format(fpr, digits = 3))),
                                                  collapse = " ")))
      logging::loginfo(paste("Acceptance:", paste(c(rbind(names(accept), accept)),
                                                  collapse = " ")))
      logging::loginfo(paste("Viability: ",
                             paste(c(rbind(names(viable_fpr_at_boundary),
                                           viable_fpr_at_boundary)), collapse = " ")))
    }

    # If all false positive rates are within the acceptance margin, return the
    # current parameters.
    if (all(accept | !viable_fpr_at_boundary)) {
      msg <- "Iterative calibration succeeded"
      if (any(!viable_fpr_at_boundary)) {
        msg <- paste(msg, "except for non-viable parameters:",
                     paste(names(candidate_penalties)[!viable_fpr_at_boundary],
                           collapse = ", "))
        candidate_penalties[!viable_fpr_at_boundary] <- NA
      }
      if (!is.null(logfile))
        logging::loginfo(msg)
      return(candidate_penalties)
    }

    # Guarantee an eventual exit.
    if (any(viable_fpr_at_boundary &
            (candidate_penalties <= lower_boundary | candidate_penalties >= upper_boundary))) {
      if (!is.null(logfile))
        logging::loginfo("Iterative calibration attempt failed: boundary was hit")
      return(NA)
    }

    # If any false positive rates exceed the target + margin, increment the
    # corresponding candidate penalties.
    to_increment <- !accept & viable_fpr_at_boundary & fpr > target_fpr
    if (any(to_increment))
      sink <- sapply(names(candidate_penalties[to_increment]), FUN = function(type) {
        candidate_penalties[type] <<-
          min(increment_factor * candidate_penalties[type], upper_boundary)
      })

    # If any false positive rates fall below the target - margin, decrement the
    # corresponding candidate penalties.
    to_decrement <- !accept & viable_fpr_at_boundary & fpr < target_fpr
    if (any(to_decrement))
      sink <- sapply(names(candidate_penalties[to_decrement]), FUN = function(type) {
        candidate_penalties[type] <<-
          max(decrement_factor * candidate_penalties[type], lower_boundary)
      })
  }
}
