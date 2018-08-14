#' Summarise the results of a batch of synthetic experiments
#'
#' @param batch_output
#' A list of lists of executed \code{synthetic_experiment} objects, as returned
#' by the \code{batch_experiment} function.
#' @param metrics
#' (Optional) A list of metric calculation functions. If missing, all metrics
#' will be included.
#' @param aggregated
#' A logical flag. If \code{TRUE} the return value is a matrix with metrics
#' down the rows and corruption types across the columns. Otherwise an array is
#' returned containing additional dimensions for the dataset and index of the
#' realised corruption. Defaults to \code{TRUE}.
#' @param count_attr
#' The name of the "count" attribute used to store the number of terms in the
#' computation of the certain metrics (not including \code{NA}s). The default value is
#' \code{count}.
#'
#' @return A data frame summarising the results of the batch experiment or, if
#' aggregated is \code{FALSE}, a list with two array elements containing,
#' respectively, the disaggregated metric and count results (from which the
#' aggregated results are calculated).
#'
#' @seealso \code{\link{batch_experiment}}
#'
#' @export
batch_summary <- function(batch_output, metrics, aggregated = TRUE,
                          count_attr = "count") {

  if (missing(metrics))
    metrics <- list("Type Precision" = metric_type_precision,
                    "Type Recall" = metric_type_recall,
                    "Hamming Distance" = purrr::partial(metric_hamming_distance,
                                                        count_attr = count_attr),
                    "Column Accuracy" = metric_column_accuracy,
                    "Parameter RMSE" = purrr::partial(metric_parameter_RMSE,
                                                      count_attr = count_attr),
                    "Parameter Accuracy" = purrr::partial(metric_parameter_accuracy,
                                                          count_attr = count_attr))

  # - NOTE: we use the "count" attributes to appropriately weight the average across
  #   different corruptions in a sub-batch, and across different data tables
  #   in a batch.

  data_names <- names(batch_output)

  # Identify all relevant patch types.
  all_corruption_types <- unique(unlist(purrr::map(data_names, .f = function(data_name) {
    sapply(batch_output[[data_name]], FUN = function(output) {
      # Ignore any experiments which failed to execute.
      if (!is_synthetic_experiment(output) && is.na(output))
        return(NA)
      if (!is_synthetic_experiment(output))
        stop("Invalid batch output: elements must be synthetic experiment objects.")
      if (!executed(output))
        return(NA)
      patch_type(output$get_corruption(), short = TRUE, unique = TRUE)
    })
  })))
  all_corruption_types <- all_corruption_types[!is.na(all_corruption_types)]
  non_trivial_corruption_types <- setdiff(all_corruption_types,
                                          patch_type(patch_identity(), short = TRUE))

  # We want two arrays with the following dimensions:
  # 1. data_name
  # 2. corruption (integer index)
  # 3. elementary patch type
  # 4. metric

  corruptions_count <- unique(unname(purrr::map_int(batch_output, length)))
  if (length(corruptions_count) != 1)
    stop("Ambiguous corruptions count")

  dimnames = list(data_names, 1:corruptions_count,
                  non_trivial_corruption_types, names(metrics))
  names(dimnames) <- c("dataset", "corruption", "type", "metric")
  arr_metric_values <- array(NA, dim = purrr::map_int(dimnames, length),
                             dimnames = dimnames)
  arr_metric_count_attrs <- array(NA, dim = purrr::map_int(dimnames, length),
                                  dimnames = dimnames)

  # The first array will contain the metric results, the second will contain the
  # "count" attributes (required for proper aggregation).

  erroneous_result_types <- character(0)
  extract_metric_value <- function(metric_result, type, metric_name) {

    if (length(names(metric_result)) != length(metric_result))
      stop("Failed to extract metric value due to missing element names.")

    types <- names(metric_result)
    if (!all(types %in% non_trivial_corruption_types)) {
      # old
      # message(paste("Ignoring result patch type(s):",
      #               paste(setdiff(types, non_trivial_corruption_types), collapse = ", ")))
      erroneous_result_types <<- unique(union(erroneous_result_types,
                                              setdiff(types, non_trivial_corruption_types)))
      types <- intersect(types, non_trivial_corruption_types)
    }

    if (!(type %in% types))
      return(NA)

    # Note that we wish to preserve the count attribute (if such exists).

    # If the metric_result is not a list then it must be the type-named scalar
    # we want (including count attribute if applicable).
    if (!is.list(metric_result)) {

      #### TEMPORARY FIX.
      # The metric_column_accuracy now returns a vector with an element for each
      # relevant corruption type.
      if (identical(metrics[[metric_name]], metric_column_accuracy) &&
          type %in% names(metric_result))
        return(metric_result[type])

      if (length(metric_result) != 1 || !identical(names(metric_result), type))
        stop("Failed to extract metric value: metric result is non-list & non-scalar")
      return(metric_result)
    }

    # In case the result is a list, restrict attention to the relevant patch type.
    metric_result <- metric_result[[type]]

    if (is.list(metric_result))
      stop("Failed to extract metric value. Metric result is a nested list.")

    # This is the case of (potentially) multiple relevant parameters.

    # Just return the average value in this case, but multiply the count attribute
    # by the number of parameters (since this counts the number of relevant terms).
    ret <- mean(metric_result)
    names(ret) <- type

    count_factor <- sum(!is.na(metric_result))
    attr(ret, which = count_attr) <- attr(metric_result, which = count_attr) *
      count_factor
    ret
  }

  # Set up a progress bar.
  .pb <- progress::progress_bar$new(total = length(data_names) *
                                      length(batch_output[[1]]))

  # Fill in the array elements.
  sink <- sapply(data_names, FUN = function(data_name) {

    sapply(1:length(batch_output[[data_name]]), FUN = function(i) {

      .pb$tick()
      output <- batch_output[[data_name]][[i]]

      # If the experiment output is not executed (or is NA), do nothing. This
      #represents the case in which there was an error during execution of the
      # experiment (i.e. the corruption is incompatible with the data).
      if (!is_synthetic_experiment(output) && is.na(output))
        return()
      if (!executed(output))
        return()

      if (is_identity_patch(output$get_corruption()))
        return()

      sapply(non_trivial_corruption_types, FUN = function(type) {

        metric_results <- lapply(metrics, FUN = function(metric) {
          if (!is_synthetic_experiment(output) && is.na(output))
            return(NA)
          if (!executed(output))
            return(NA)
          metric(output)
        })
        sapply(names(metric_results), FUN = function(metric_name) {

          metric_result <- metric_results[[metric_name]]

          # If the computed metric is empty or NA, do nothing.
          if (length(metric_result) == 0 || is.na(metric_result))
            return()

          extracted <- extract_metric_value(metric_result,
                                            type = type, metric_name = metric_name)
          arr_metric_values[data_name, i, type, metric_name] <<-
            as.numeric(extracted)

          count_attr_value <- attr(extracted, which = count_attr)
          if (!is.null(count_attr_value)) {

            # If the extracted value is NA, expect an empty or zero count
            # attribute (and do nothing).
            if (is.na(extracted)) {
              if (length(count_attr_value) != 0 && count_attr_value != 0)
                stop("NA metric value with non-empty count attribute")
              return()
            }

            arr_metric_count_attrs[data_name, i, type, metric_name] <<-
              count_attr_value
          }
        })
      })
    })
  })

  if (length(erroneous_result_types) != 0)
    message(paste("Ignored erroneous result patch type(s):",
                  paste(erroneous_result_types, collapse = ", ")))

  if (!aggregated) {
    l <- list(arr_metric_values, arr_metric_count_attrs)
    names(l) <- c("metrics", "count")
    return(l)
  }

  # Aggregate over the experiments (i.e. different corruptions) and over the
  # datasets, taking a weighted average in case there is "count" information.
  dn <- dimnames(arr_metric_values)
  stopifnot(setequal(dn[[3]], non_trivial_corruption_types))
  stopifnot(setequal(dn[[4]], names(metrics)))

  ret <- sapply(non_trivial_corruption_types, FUN = function(type) {
    sapply(names(metrics), FUN = function(metric) {

      values <- arr_metric_values[, , type, metric]
      counts <- arr_metric_count_attrs[, , type, metric]
      if (all(is.na(counts)))
        return(mean(values, na.rm = TRUE))

      if (!identical(is.na(values), is.na(counts)))
        stop("Incompatible counts and values")

      # Replace all NA values with zeros.
      values[is.na(values)] <- 0
      counts[is.na(counts)] <- 0
      (as.vector(values) %*% as.vector(counts))/sum(counts)
    })
  })

  # The mean function returns NaN if all elements are NA, so replace any NaN
  # values with NA.
  ret[is.nan(ret)] <- NA
  ret
}
