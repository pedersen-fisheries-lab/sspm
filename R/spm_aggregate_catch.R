#' Update biomass value from catch adta
#'
#' Aggregate the catch data contained in a catch dataset and update the biomass
#' dataset with the subtracted catch.
#'
#' @param biomass **\[sspm_dataset (smoothed)\]** The dataset containing the
#'     biomass variable.
#' @param catch **\[sspm_dataset\]** The dataset containing the catch variable.
#' @param biomass_variable **\[character\]** The biomass variab of `biomass`.
#' @param catch_variable **\[character\]** The catch column of `catch`.
#' @param corrections **\[data.frame\]** Optional landings corrections.
#' @inheritParams spm_aggregate
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
                          corrections = NULL,
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
                   corrections = NULL,
                   fun = sum,
                   group_by = "spacetime",
                   fill,
                   apply_to_df = FALSE,
                   ...){

            if (!checkmate::test_subset(biomass_variable,
                                        names(spm_smoothed_data(biomass)))) {
              stop("`biomass_variable` must be a column of the smoothed data of `biomass`",
                   call. = FALSE)
            }

            if (!checkmate::test_subset(catch_variable,
                                        names(spm_data(catch)))) {
              stop("`catch_variable` must be a column of `catch`",
                   call. = FALSE)
            }

            info_message <-
              paste0(" Offsetting biomass with catch data using columns: ",
                     paste(cli::col_green(c(biomass_variable, catch_variable)), collapse = ", "))
            cli::cli_alert_info(info_message)

            # Get the right columns
            biomass_time_col <- spm_time_column(biomass)
            catch_time_col <- spm_time_column(catch)

            # browser()

            # Aggregate the catch
            catch <- spm_aggregate(dataset = catch,
                                   boundaries = spm_boundaries(biomass),
                                   variable = catch_variable,
                                   fun = fun, group_by = group_by,
                                   fill = fill, apply_to_df = apply_to_df,
                                   na.rm = TRUE, ...)
            catch_data <- spm_data(catch) %>%
              dplyr::rename(!!biomass_time_col := catch_time_col)

            # First, join data
            full_smoothed_data <- spm_smoothed_data(biomass) %>%
              dplyr::mutate(!!biomass_time_col :=
                              as.factor(.data[[biomass_time_col]])) %>%
              dplyr::left_join(catch_data,
                               by = sapply(c(biomass_time_col, "patch_id"),
                                           rlang::as_string))

            # Use corrections if need be
            if(!is.null(corrections)){
              # TODO: add verification for time and boundary cols and catch_adjustment
              nrow_data <- nrow(full_smoothed_data)
              full_smoothed_data <- full_smoothed_data %>%
                left_join(corrections) %>%
                mutate(catch = catch_adjustment * catch) %>%
                select(-catch_adjustment)
              checkmate::assert_true(nrow(full_smoothed_data) == nrow_data)
            }

            # Make correct names
            catch_name <- paste(c(biomass_variable, spm_name(biomass),
                                  "with_catch"), collapse = "_")
            change_name <- paste0(c(biomass_variable, spm_name(biomass),
                                    "with_catch_change"), collapse = "_")

            # Calculate
            full_smoothed_data <- full_smoothed_data %>%

              dplyr::group_by(.data[["patch_id"]],
                              !!spm_boundary_colum(spm_boundaries(biomass))) %>%

              dplyr::mutate(area_no_units = as.vector(.data$area)) %>%
              dplyr::mutate(
                !!catch_name :=
                  (.data[[biomass_variable]] + .data[[catch_variable]] / .data$area_km2)) %>%
              dplyr::mutate(
                !!change_name :=
                  log(.data[[catch_name]]) - log(dplyr::lag(.data[[catch_name]],
                                                            default = NA))) %>%

              dplyr::select(-.data$area_no_units) %>%
              dplyr::ungroup() %>%

              # Replace NAs with 0s (disabled to fit script behavior better)
              # dplyr::mutate(!!change_name := ifelse(is.na(.data[[change_name]]),
              #                                       0, .data[[change_name]])) %>%

              dplyr::relocate(dplyr::starts_with(biomass_variable),
                              .after = .data$row_ID) %>%
              dplyr::mutate(!!biomass_time_col :=
                              as.numeric(as.character(.data[[biomass_time_col]])))

            spm_smoothed_data(biomass) <- full_smoothed_data

            return(biomass)

          }
)
