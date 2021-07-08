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
                   ...){

            checkmate::assert_character(variable)
            checkmate::assert_function(fun)
            checkmate::assert_choice(group_by, spm_aggregation_choices())

            time_col <- spm_time_column(dataset)
            if (!is_mapped(dataset)) {
              # Need to map dataset
              dataset <- join_datasets(dataset, boundaries)
            }

            dataset_data <- spm_data(dataset)

            if (group_by == "spacetime"){

              dataset_data_tmp <- spm_data(dataset) %>%
                dplyr::group_by(.data[[time_col]], .data$patch_id)

            } else if (group_by == "space"){

              dataset_data_tmp <- spm_data(dataset) %>%
                dplyr::group_by(.data$patch_id)

            } else if (group_by == "time"){

              dataset_data_tmp <- spm_data(dataset) %>%
                dplyr::group_by(.data[[time_col]])

            }

            args_list <- list(...)

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

            dataset_data_tmp <- dataset_data_tmp %>%
              sf::st_set_geometry(NULL) %>%
              dplyr::group_modify(.f = the_fun,
                                  ... = list(variable = variable,
                                             fun = fun,
                                             args = args_list)) %>%
              dplyr::ungroup() %>%
              unique()

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

            if (do_completion) {
              dataset_data_tmp <- dataset_data_tmp %>%
                tidyr::complete(.data[[time_col]], .data$patch_id,
                                fill = list(temp = fill_value))
            }

            spm_data(dataset) <- dataset_data_tmp %>%
              dplyr::rename(!!variable := .data$temp)

            return(dataset)

          }
)
