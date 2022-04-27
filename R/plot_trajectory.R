#' Plot trajectory of exploitation rate with biomass
#'
#' Provides a trajectory plot for exploitation rate (ER), defined as the ratio
#' between catch of a given year and fishable biomass from the previous year,
#' and that fishable biomass.
#'
#' @inheritParams spm_aggregate
#' @param dataset **\[sspm_dataset\]** Corresponding biomass dataset.
#' @param biomass **\[character\]** Biomass variable for plotting.
#' @param catch **\[character\]** Catch variable for plotting
#' @param dataset_biomass **\[character\]** Biomass variable for plotting in
#'    the biomass dataset if the variable name is different there.
#'
#' @return
#' A ggplot2 plot object.
#'
#' @examples
#' \dontrun{
#' plot_trajectory(sspm_fit, borealis_dataset_fitted,
#'                 "weight_per_km2", "catch")
#' }
#'
#' @export
#' @rdname plot_trajectory
setGeneric(name = "plot_trajectory",
           def = function(sspm_object,
                          dataset,
                          biomass,
                          catch,
                          ...){

             standardGeneric("plot_trajectory")
           }
)

# Methods -----------------------------------------------------------------
#' @export
#' @rdname plot_trajectory
setMethod(f = "plot_trajectory",
          signature(sspm_object = "sspm_fit",
                    dataset = "sspm_dataset",
                    biomass = "character",
                    catch = "character"),
          function(sspm_object,
                   dataset,
                   biomass,
                   catch,
                   dataset_biomass = NULL){

            # Get smoothed data
            smoothed_data <- spm_smoothed_data(sspm_object)
            bounds <- spm_boundaries(sspm_object)
            boundary <- spm_boundary(bounds)
            time <- spm_time(sspm_object)
            patches_area <- spm_patches_area(bounds)

            if (is.null(dataset_biomass)) dataset_biomass <- biomass

            # TODO assert column biomass, catch
            biomass_actual <- sspm_object %>%
              spm_smoothed_data() %>%
              dplyr::mutate(biomass = .data[[dataset_biomass]] *
                              .data[[patches_area]]) %>%
              dplyr::group_by(.data[[time]], .data[[boundary]]) %>%
              dplyr::summarise(biomass = sum(.data$biomass),
                               catch = sum(.data[[catch]])) %>%
              sf::st_drop_geometry() %>%
              dplyr::ungroup()

            biomass_actual <- biomass_actual %>%
              dplyr::group_by(.data[[boundary]]) %>%
              dplyr::mutate(catch = as.numeric(.data[[catch]]),
                            biomass = as.numeric(.data$biomass),
                            biomass_w_catch = .data$biomass + .data[[catch]]) %>%
              dplyr::mutate(year_lag =
                              dplyr::lag(.data[[time]], n = 1),
                            biomass_lag =
                              dplyr::lag(.data$biomass, n = 1),
                            biomass_w_catch_lag =
                              dplyr::lag(.data$biomass_w_catch, n = 1),
                            catch_lag =
                              dplyr::lag(.data[[catch]], n = 1)) %>%
              dplyr::mutate(ER = .data[[catch]]/.data$biomass_lag) %>%
              dplyr::mutate(color = "Actual")

            # -----------------------------------------------------------

            biomass_preds <-
              predict(sspm_object, biomass = biomass, aggregate = TRUE,
                      interval = FALSE, next_ts = FALSE) %>%
              sf::st_drop_geometry()
            biomass_preds_next_ts <-
              predict(sspm_object, biomass = biomass, aggregate = TRUE,
                      interval = FALSE, next_ts = TRUE) %>%
              sf::st_drop_geometry()

            all_preds <- biomass_preds %>%
              dplyr::bind_rows(biomass_preds_next_ts) %>%
              dplyr::ungroup()

            catch_dat <- smoothed_data %>%
              sf::st_drop_geometry() %>%
              dplyr::select(.data[[boundary]], .data[[time]],
                            .data[[catch]]) %>%
              dplyr::group_by(.data[[boundary]], .data[[time]]) %>%
              dplyr::summarise(catch = sum(.data[[catch]])) %>%
              dplyr::ungroup()

            all_preds_w_catch <- all_preds %>%
              dplyr::left_join(catch_dat, by = c("sfa", "year_f")) %>%
              dplyr::group_by(.data[[boundary]]) %>%
              dplyr::mutate(catch = as.numeric(.data[[catch]]),
                            biomass = as.numeric(.data$biomass),
                            biomass_w_catch = .data$biomass + .data[[catch]]) %>%
              dplyr::mutate(year_lag =
                              dplyr::lag(.data[[time]], n = 1),
                            biomass_lag =
                              dplyr::lag(.data$biomass, n = 1),
                            biomass_w_catch_lag =
                              dplyr::lag(.data$biomass_w_catch, n = 1),
                            catch_lag =
                              dplyr::lag(.data[[catch]], n = 1)) %>%
              dplyr::mutate(ER = .data[[catch]]/.data$biomass_lag) %>%
              dplyr::mutate(color = "Predictions")

            # stopifnot(all((all_data$biomass_w_catch_lag > all_data$catch), na.rm = T))
            # stopifnot(all((all_data$biomass_w_catch_lag > all_data$catch), na.rm = T))

            all_data <- all_preds_w_catch %>%
              dplyr::bind_rows(biomass_actual)

            all_data %>%
              dplyr::filter(.data$ER > 0) %>%
              dplyr::mutate(yr_label = paste0(.data$year_lag, " - ", .data[[time]])) %>%
              # filter(year_f >= 2005) %>%
              # filter(year_f <= 2014) %>%
              ggplot2::ggplot() +
              ggplot2::geom_point(ggplot2::aes(x = .data$biomass_lag, y = .data$ER,
                                               color = .data$color)) +
              ggplot2::geom_path(ggplot2::aes(x = .data$biomass_lag, y = .data$ER,
                                              color = .data$color)) +
              ggplot2::facet_wrap(~sfa, scales = "free") +
              ggplot2::scale_color_manual(values = c("Actual" = "black",
                                                     "Predictions" = "red")) +
              ggplot2::theme_light() +
              ggplot2::labs(color = "Type") +
              ggplot2::geom_text(ggplot2::aes(x = .data$biomass_lag, y = .data$ER,
                                              color = .data$color, label = .data$yr_label),
                                 hjust = 0, vjust = 0)

          }
)
