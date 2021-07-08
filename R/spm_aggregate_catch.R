#' Update biomass value from catch adta
#'
#' Aggregate the catch data contained in a catch dataset and update the biomass
#' dataset with the subtracted catch.
#'
#' @inheritParams spm_aggregate
#' @param biomass **\[sspm_dataset (smoothed)\]** The dataset containing the
#'     biomass variable.
#' @param catch **\[sspm_dataset\]** The dataset containing the catch variable.
#' @param biomass_variable **\[character\]** The biomass variab of `biomass`.
#' @param catch_variable **\[character\]** The catch column of `catch`.
#'
#' @return
#' Updated `sspm_dataset`.
#'
#' @export
setGeneric(name = "spm_aggregate_catch",
           def = function(biomass,
                          catch,
                          biomass_variable,
                          catch_variable,
                          fun = sum,
                          group_by = "spacetime",
                          fill,
                          apply_to_df = FALSE,
                          ...){
             standardGeneric("spm_aggregate_catch")
           }
)

#' @rdname spm_aggregate_catch
#' @export
setMethod(f = "spm_aggregate_catch",
          signature(biomass = "sspm_dataset",
                    catch = "sspm_dataset",
                    biomass_variable = "character",
                    catch_variable = "character"),
          function(biomass,
                   catch,
                   biomass_variable,
                   catch_variable,
                   fun = sum,
                   group_by = "spacetime",
                   fill,
                   apply_to_df = FALSE,
                   ...){

            info_message <-
              paste0(" Offsetting biomass with catch data using columns: ",
                     paste(cli::col_green(c(biomass_variable, catch_variable)), collapse = ", "))
            cli::cli_alert_info(info_message)

            catch <- spm_aggregate(dataset = catch,
                                   boundaries = spm_boundaries(biomass),
                                   variable = catch_variable,
                                   fun = fun, group_by = group_by,
                                   fill = fill, apply_to_df = apply_to_df,
                                   na.rm = TRUE, ...)

            # Calculate the right columns
            smoothed_data_time_col <- spm_time_column(biomass)
            time_col <- spm_time_column(catch)

            # First, join data
            full_smoothed_data <- spm_data(biomass) %>%
              dplyr::mutate(!!smoothed_data_time_col :=
                              as.factor(.data[[smoothed_data_time_col]])) %>%
              dplyr::rename(!!time_col := smoothed_data_time_col) %>%
              dplyr::left_join(spm_data(catch),
                               by = sapply(c(time_col, "patch_id"),
                                           rlang::as_string))

            catch_name <- paste0(biomass_variable, "_with_catch")
            change_name <- paste0(biomass_variable, "_with_catch_change")

            full_smoothed_data <- full_smoothed_data %>%
              dplyr::group_by(.data[["patch_id"]],
                              !!spm_boundary_colum(spm_boundaries(biomass))) %>%
              dplyr::mutate(
                !!catch_name :=
                  .data[[biomass_variable]] + .data / .data$area) %>%
              dplyr::mutate(
                !!change_name :=
                  log(.data[[catch_name]]) - log(dplyr::lag(.data[[biomass_variable]],
                                                            default = NA))) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(!!change_name := ifelse(is.na(.data[[change_name]]),
                                                    0, .data[[change_name]])) %>%
              dplyr::relocate(dplyr::starts_with(biomass_variable),
                              .after = .data$row_ID)

          }
)
