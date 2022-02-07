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
                          boundaries = NULL,
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
                    boundaries = "sspm_discrete_boundaryOrNULL"),
          function(dataset,
                   boundaries,
                   variable,
                   fun,
                   group_by,
                   fill,
                   apply_to_df,
                   ...){

            # Get info
            checkmate::assert_character(variable)
            checkmate::assert_function(fun)
            checkmate::assert_choice(group_by, spm_aggregation_choices())

            time_col <- spm_time(dataset)
            if (!is_mapped(dataset)) { # Need to map dataset
              dataset <- join_datasets(dataset, boundaries)
            }

            # Get data
            dataset_data <- spm_data(dataset)

            # Group data
            dataset_data_tmp <- group_data(dataset_data, group_by, time_col)

            args_list <- list(...)

            # Two ways to aggregate: by dataframe or by list. We get the correct
            # meta-function from this function
            the_fun <- get_apply_fun(apply_to_df)

            # Then we apply it
            dataset_data_tmp <- dataset_data_tmp %>%
              sf::st_set_geometry(NULL) %>%
              dplyr::group_modify(.f = the_fun,
                                  ... = list(variable = variable,
                                             fun = fun,
                                             args = args_list)) %>%
              dplyr::ungroup() %>%
              unique()

            # We determine how best to clean up if need be
            if (checkmate::test_logical(fill)) {
              if (is.na(fill)){
                do_completion <- TRUE
                fill_value <- fill
              } else {
                if (fill) {
                  do_completion <- FALSE
                  warning("No fill values provided, completion skipped")
                } else {
                  do_completion <- FALSE
                }
              }
            } else {
              if (checkmate::test_function(fill)){
                do_completion <- TRUE
                fill_value <- do.call(fill,
                                      append(list(dataset_data[[variable]]),
                                             args_list))
              } else {
                do_completion <- TRUE
                fill_value <- fill
              }
            }

            # If we need to complete, we go ahead and do so
            if (do_completion) {
              dataset_data_tmp <- dataset_data_tmp %>%
                tidyr::complete(.data[[time_col]], .data$patch_id,
                                fill = list(temp = fill_value))
            }

            # Rename before returning
            spm_data(dataset) <- dataset_data_tmp %>%
              dplyr::rename(!!variable := .data$temp)

            return(dataset)

          }
)

# Helpers -----------------------------------------------------------------

# This functions takes care of applying the proper grouping onto the data
group_data <- function(dataset_data, group_by, time_col){
  if (group_by == "spacetime"){

    grouped_data <- dataset_data %>%
      dplyr::group_by(.data[[time_col]], .data$patch_id)

  } else if (group_by == "space"){

    grouped_data <- dataset_data %>%
      dplyr::group_by(.data$patch_id)

  } else if (group_by == "time"){

    grouped_data <- dataset_data %>%
      dplyr::group_by(.data[[time_col]])

  }
  return(grouped_data)
}

# Retrurns on demand one of two functions for aggregation
get_apply_fun <- function(apply_to_df){

  if (apply_to_df) {

    the_fun <- function(df, groups, ...){
      all_args <- list(...)[[1]]
      data.frame(do.call(all_args$fun,
                         append(list(df), all_args$args)))
    }

  } else {

    the_fun <- function(df, groups, ...){
      all_args <- list(...)[[1]]
      data.frame(temp = do.call(all_args$fun,
                                append(list(df[[all_args$variable]]),
                                       all_args$args)))
    }

  }

  return(the_fun)
}
