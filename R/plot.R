#' Plot `sspm` objects
#'
#' Plot methods for a range of sspm objects.
#'
#' @param x **\[sspm_...\]** An object from this package.
#' @param y NOT USED (from generic).
#' @param ... NOT USED (from generic).
#' @param smoothed_var **\[character\]** Variable to plot.
#' @param page **\[character\]** Either "first" for the firs page of plots, or
#'     "all" for all pages
#' @param nrow **\[numeric\]** The number of rows to paginate the plot on.
#' @param ncol **\[numeric\]** The number of columns to paginate the plot on.
#' @param log **\[logical\]** Whether to plot on a log scale, default to TRUE.
#' @inheritParams spm_predict
#' @param biomass_var **\[character\]** Biomass variable to plot.
#' @param biomass_var_predict **\[character\]** Biomass variable to plot (from
#'      predictions, optionnal).
#' @param biomass_var_smooth **\[character\]** Biomass variable to plot (smooth,
#'      optional).
#' @param biomass_var_origin **\[character\]** Biomass variable to plot (from
#'      original dataset, optionnal)
#' @param use_sf **\[logical\]** Whether to produce a spatial plot.
#'
#' @return
#' A ggplot object.
#'
#' @export
#' @name plot
#' @aliases plot.sspm

NULL

#' @rdname plot
setMethod("plot",
          signature(x = "sspm_boundary",
                    y = "missing"),
          definition = function(x, y, ...) {

            boundaries <- spm_boundaries(x)
            boundary_column <- spm_boundary_column(x)

            if (checkmate::test_class(x, "sspm_discrete_boundary")) {

              patches <- x@patches
              points <- x@points

              sspm_discrete_plot <- ggplot2::ggplot() +
                ggplot2::geom_sf(data = patches,
                                 fill = NA, col = "#36454F") +
                ggplot2::geom_sf(data = boundaries,
                                 ggplot2::aes(col = .data[[boundary_column]]),
                                 fill = NA) +
                ggplot2::scale_color_viridis_d(boundary_column) +
                ggplot2::theme_light()

              if(!is.null(points)){
                sspm_discrete_plot <- sspm_discrete_plot +
                  ggplot2::geom_sf(data = points, col = "#6082B6")
              }

            } else if (checkmate::test_class(x, "sspm_boundary")) {

              sspm_discrete_plot <- ggplot2::ggplot() +
                ggplot2::geom_sf(data = boundaries,
                                 ggplot2::aes(fill = .data[[boundary_column]]),
                                 col = "#36454F") +
                ggplot2::scale_fill_viridis_d(boundary_column) +
                ggplot2::theme_light()

            }

            return(sspm_discrete_plot)

          }
)

#' @export
#' @rdname plot
setMethod("plot",
          signature(x = "sspm_dataset",
                    y = "missing"),
          definition = function(x, y, ..., smoothed_var = NULL,
                                page = "first", nrow = 2, ncol = 4, log = TRUE) {

            smoothed_data <- spm_smoothed_data(x)

            if (is.null(smoothed_var)) {

              sspm_discrete_plot <- plot(spm_boundaries(x))
              show(sspm_discrete_plot)

            } else {

              if (!checkmate::test_subset(smoothed_var, names(smoothed_data))) {
                stop("`smoothed_var` must be a column of the smoothed data", call. = FALSE)
              }

              time_col <- spm_time_column(x)

              sspm_discrete_plot <- spm_plot_routine(smoothed_data, smoothed_var,
                                                     page, nrow, ncol, time_col, log)

              return(sspm_discrete_plot)
            }

          }
)

#' @export
#' @rdname plot
setMethod("plot",
          signature(x = "sspm_fit",
                    y = "missing"),
          definition = function(x, y, ..., smoothed_var = NULL,
                                biomass = NULL, use_sf = TRUE,
                                biomass_var = NULL, biomass_var_predict = NULL,
                                biomass_var_smooth = NULL, biomass_var_origin = NULL,
                                page = "first", nrow = 2, ncol = 4, log = TRUE) {

            if (is.null(biomass)){

              smoothed_data <- spm_smoothed_data(x)

              if (!is.null(smoothed_var)) {

                if (!checkmate::test_subset(smoothed_var, names(smoothed_data))) {
                  stop("`smoothed_var` must be a column of the smoothed data", call. = FALSE)
                }

                time_col <- spm_time_column(x)

                sspm_discrete_plot <- spm_plot_routine(smoothed_data, smoothed_var,
                                                       page, nrow, ncol, time_col)

              } else {

                preds <- spm_predict(x)$pred
                response <- spm_response(spm_formulas(x))
                smoothed_data_with_preds <- smoothed_data %>%
                  dplyr::mutate(predicted = preds,
                                color = ifelse(.data$train_test, "TRAIN", "TEST"))

                sspm_discrete_plot <-
                  ggplot2::ggplot(data = smoothed_data_with_preds) +
                  ggplot2::geom_point(ggplot2::aes(x = exp(.data[[response]]),
                                                   y = .data$predicted,
                                                   col = .data$color)) +
                  ggplot2::theme_light() +
                  ggplot2::labs(x = "actual") +
                  ggplot2::scale_color_viridis_d("Set") +
                  ggplot2::facet_wrap(~.data[[spm_boundary_column(x)]])

              }

              return(sspm_discrete_plot)

            } else {

              if (checkmate::test_class(biomass, "character")){

                biomass_preds <- spm_predict_biomass(x, biomass)

                smoothed_data <- spm_smoothed_data(x) %>%
                  dplyr::mutate(area = units::set_units(st_area(.data$geometry),
                                                        value = "km^2")) %>%
                  dplyr::mutate(biomass = (.data[[biomass]] * as.numeric(.data$area)))
                time_col <- spm_time_column(x)

                if (log) {
                  biomass_preds$biomass_pred <- log(biomass_preds$biomass)
                  smoothed_data$biomass <- log(smoothed_data$biomass)
                  the_title <- "Biomass (logged)"
                } else {
                  the_title <- "Biomass"
                }

                if (use_sf){

                  biomass_plot <- ggplot2::ggplot(data = biomass_preds) +
                    ggplot2::geom_sf(ggplot2::aes(fill = .data$biomass_pred)) +
                    ggforce::facet_wrap_paginate(~ .data[[time_col]],
                                                 nrow = nrow, ncol = ncol,
                                                 page = page) +
                    ggplot2::scale_fill_viridis_c() +
                    ggplot2::labs(fill = the_title)

                } else {

                  biomass_plot <- biomass_preds %>%
                    ggplot2::ggplot(ggplot2::aes(x = .data[[time_col]],
                                                 y = .data$biomass_pred)) +
                    ggplot2::geom_line(color = "red") +
                    ggforce::facet_wrap_paginate(~patch_id,
                                                 nrow = nrow, ncol = ncol,
                                                 page = page, scales = "free") +
                    ggplot2::geom_line(data = smoothed_data,
                                       ggplot2::aes(x = .data[[time_col]],
                                                    y = .data$biomass),
                                       color = "blue" )
                }

                return(biomass_plot)

              } else if (checkmate::test_class(biomass, "sspm_dataset")){

                if (is.null(biomass_var_predict)) {
                  biomass_var_predict <- paste0(biomass_var, "_smooth")
                }

                if (is.null(biomass_var_smooth)) {
                  biomass_var_smooth <- paste0(biomass_var, "_smooth")
                }

                if (is.null(biomass_var_origin)) {
                  biomass_var_origin <- biomass_var
                }

                biomass_preds <- spm_predict_biomass(x, biomass_var_predict)
                boundary_col <- spm_boundary_column(x)
                time_col <- spm_time_column(x)

                # biomass_smooth_summary <- spm_smoothed_data(biomass) %>%
                #   dplyr::mutate(area = as.numeric(units::set_units(st_area(.data$geometry),
                #                                                    value = "km^2")),
                #                 biomass = .data[[biomass_var_smooth]] * .data$area) %>%
                #   dplyr::group_by(.data[[boundary_col]], .data[[time_col]]) %>%
                #   dplyr::summarise(biomass_sum = sum(biomass)) %>%
                #   dplyr::mutate(!!time_col := as.numeric(as.character(.data[[time_col]])))

                biomass_preds <- biomass_preds %>%
                  dplyr::group_by(.data[[boundary_col]], .data[[time_col]]) %>%
                  dplyr::summarise(biomass_sum = sum(.data$biomass)) %>%
                  dplyr::mutate(type = "Predictions")

                biomass_actual <- spm_data(biomass) %>%
                  dplyr::group_by(.data[[boundary_col]], .data$patch_id, .data[[time_col]]) %>%
                  dplyr::summarise(biomass_mean = mean(.data[[biomass_var_origin]])) %>%

                  dplyr::mutate(area = as.numeric(units::set_units(st_area(.data$geometry),
                                                                   value = "km^2")),
                                biomass = .data$biomass_mean * .data$area) %>%

                  dplyr::group_by(.data[[boundary_col]], .data[[time_col]]) %>%
                  dplyr::summarise(biomass_sum = sum(.data$biomass)) %>%
                  dplyr::mutate(!!time_col := as.numeric(as.character(.data[[time_col]])))  %>%
                  dplyr::mutate(type = "Actual")

                biomass_plot <- biomass_preds %>%
                  dplyr::bind_rows(biomass_actual) %>%
                  dplyr::filter(.data$year_f %in% c(2006:2020)) %>%
                  ggplot2::ggplot() +
                  # ggplot2::geom_line(color = "red") +
                  ggplot2::facet_wrap(~sfa, scales = "free") +
                  ggplot2::geom_line(ggplot2::aes(x = .data[[time_col]],
                                                  y = .data$biomass_sum,
                                                  color = .data$type,
                                                  linetype = .data$type)) +

                  # ggplot2::geom_line(data = biomass_smooth_summary,
                  #                    ggplot2:: aes(x = .data[[time_col]],
                  #                                  y = .data$biomass_sum), col = "blue") +

                  ggplot2::scale_y_log10()

                return(biomass_plot)

              }

            }

          }
)

# -------------------------------------------------------------------------

spm_plot_routine <- function(smoothed_data, smoothed_var,
                             page, nrow, ncol, time_col, log) {

  if (log) {
    smoothed_data[[smoothed_var]] <- log(smoothed_data[[smoothed_var]])
    the_title <- paste0(smoothed_var, " (log)")
  } else {
    the_title <- smoothed_var
  }

  if (is.character(page)) {

    if (page == "all") {

      time_col_levels <- length(unique(smoothed_data[[time_col]]))
      n_per_page <- nrow * ncol
      n_pages <- time_col_levels %/% (n_per_page) +
        (time_col_levels %% n_per_page > 1)

      sspm_discrete_plot <- list()

      for (page_nb in seq_len(length.out = n_pages)) {

        sspm_discrete_plot[[page_nb]] <-
          ggplot2::ggplot(data = smoothed_data) +
          ggplot2::geom_sf(ggplot2::aes(fill = .data[[smoothed_var]])) +
          ggforce::facet_wrap_paginate(~ .data[[time_col]],
                                       nrow = nrow, ncol = ncol,
                                       page = page_nb) +
          ggplot2::scale_fill_viridis_c() +
          ggplot2::labs(fill = the_title)

      }

    } else {

      sspm_discrete_plot <- ggplot2::ggplot(data = smoothed_data) +
        ggplot2::geom_sf(ggplot2::aes(fill = .data[[smoothed_var]])) +
        ggforce::facet_wrap_paginate(~ .data[[time_col]],
                                     nrow = nrow, ncol = ncol,
                                     page = 1) +
        ggplot2::scale_fill_viridis_c() +
        ggplot2::labs(fill = the_title)

    }

  } else if (is.numeric(page)) {

    sspm_discrete_plot <- ggplot2::ggplot(data = smoothed_data) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data[[smoothed_var]])) +
      ggforce::facet_wrap_paginate(~ .data[[time_col]],
                                   nrow = nrow, ncol = ncol,
                                   page = page) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::labs(fill = the_title)

  }

  return(sspm_discrete_plot)

}
