#' Aggregate a dataset variable based on a boundary
#'
#' Aggregate the data contained in a dataset based on the discretized boundaries,
#' using a function and a filling value
#'
#' @param dataset **\[sspm_dataset\]** The dataset object.
#' @param boundaries **\[sspm_discrete_boundary\]** The boundaries object
#'     (optionnal).
#' @param variable **\[character\]** Variable to aggregate (ignored in case
#'     `apply_to_df` is `TRUE`).
#' @param fun **\[function\]** Function to use to aggregate data.
#' @param group_by **\[character\]** One of `time`, `space` and `spacetime`.
#' @param fill **\[logical OR numeric OR function\]** Whether to complete the
#'     incomplete cases, default to `FALSE` for no completion.
#' @param apply_to_df **\[logical\]** Wether `fun` applied to  the data frame
#'     group or to `variable`, default to `FALSE`.
#' @param ... More arguments passed onto `fun`
#'
#' @return
#' Updated `sspm_dataset`.
#'
#' @export
#' @rdname spm_aggregate
setGeneric(name = "spm_aggregate",
           def = function(dataset,
                          boundaries,
                          level = "patch",
                          type = "data",
                          variable,
                          fun,
                          group_by = "spacetime",
                          fill = FALSE,
                          apply_to_df = FALSE,
                          ...){

             standardGeneric("spm_aggregate")
           }
)

# Methods -----------------------------------------------------------------
#' @export
#' @rdname spm_aggregate
setMethod(f = "spm_aggregate",
          signature(dataset = "sspm_dataset",
                    boundaries = "missing"),
          function(dataset,
                   boundaries,
                   level,
                   type,
                   variable,
                   fun,
                   group_by,
                   fill,
                   apply_to_df,
                   ...){

            if (is_mapped(dataset)) {
              boundaries <- spm_boundaries(dataset)
            } else {
              cli::cli_alert_danger("no boundaries provided to aggregate an un-mapped dataset")
              stop(call. = FALSE)
            }

            spm_aggregate(dataset, boundaries, level, type, variable, fun, group_by,
                          fill, apply_to_df, ...)

          }
)

#' @export
#' @rdname spm_aggregate
setMethod(f = "spm_aggregate",
          signature(dataset = "sspm_dataset",
                    boundaries = "sspm_discrete_boundary"),
          function(dataset,
                   boundaries,
                   level,
                   type,
                   variable,
                   fun,
                   group_by,
                   fill,
                   apply_to_df,
                   ...){

            # Check info
            checkmate::assert_character(variable)
            checkmate::assert_function(fun)
            checkmate::assert_choice(group_by, spm_aggregation_choices())
            checkmate::assert_choice(level, spm_aggregation_levels_choices())
            checkmate::assert_choice(type, spm_aggregation_types_choices())

            # Get data
            bounds <- spm_boundaries(dataset)
            boundary <- spm_boundary(bounds)
            time_col <- spm_time(dataset)

            if (!is_mapped(dataset)) { # Need to map dataset
              dataset <- join_datasets(dataset, boundaries)
            }

            if (type == "data"){

              dataset_data <- spm_data(dataset)
              spm_data(dataset) <-
                spm_aggregate_routine(dataset_data, boundaries, group_by, level,
                                      time_col, boundary, variable, fun, fill,
                                      apply_to_df, ...)

            } else if (type == "smoothed"){

              dataset_data <- spm_smoothed_data(dataset)
              spm_smoothed_data(dataset) <-
                spm_aggregate_routine(dataset_data, boundaries, group_by, level,
                                      time_col, boundary, variable, fun, fill,
                                      apply_to_df, ...)

            }

            return(dataset)

          }
)
