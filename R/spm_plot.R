#' Plot various `sspm` objects
#'
#' Plot methods for a range of sspm objects.
#'
#' @param sspm_object **\[sspm_...\]** An object from this package.
#' @param smoothed_var **\[character\]** Variable to plot.
#' @param page **\[character\]** Either "first" for the firs page of plots, or
#'     "all" for all pages
#' @param nrow **\[numeric\]** The number of rows to paginate the plot on.
#' @param ncol **\[numeric\]** The number of columns to paginate the plot on.
#' @param log **\[logical\]** Whether to plot on a log scale, default to TRUE.
#' @inheritParams spm_predict
#'
#' @return
#' N/A
#'
#' @export
setGeneric(name = "spm_plot",
           def = function(sspm_object, smoothed_var = NULL,
                          page = NULL, nrow = NULL, ncol = NULL, log = TRUE) {
             standardGeneric("spm_plot")
           }
)

#' @export
setGeneric(name = "spm_plot_biomass",
           def = function(sspm_object, biomass, biomass_var = NULL,
                          biomass_var_predict = NULL, biomass_var_smooth = NULL,
                          biomass_var_origin = NULL, catch, use_sf = FALSE,
                          nrow = NULL, ncol = NULL, page = NULL, log = TRUE) {
             standardGeneric("spm_plot_biomass")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm_plot
setMethod("spm_plot",
          signature(sspm_object = "sspm_boundary"),
          definition = function(sspm_object) {

            boundaries <- spm_boundaries(sspm_object)
            boundary_column <- spm_boundary_column(sspm_object)

            if (checkmate::test_class(sspm_object, "sspm_discrete_boundary")) {

              patches <- sspm_object@patches
              points <- sspm_object@points

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

            } else if (checkmate::test_class(sspm_object, "sspm_boundary")) {

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
#' @rdname spm_plot
setMethod("spm_plot",
          signature(sspm_object = "sspm_dataset"),
          definition = function(sspm_object, smoothed_var = NULL,
                                page = "first", nrow = 2, ncol = 4, log = TRUE) {

            smoothed_data <- spm_smoothed_data(sspm_object)

            if (is.null(smoothed_var)) {

              sspm_discrete_plot <- plot(spm_boundaries(sspm_object))
              show(sspm_discrete_plot)

            } else {

              if (!checkmate::test_subset(smoothed_var, names(smoothed_data))) {
                stop("`smoothed_var` must be a column of the smoothed data", call. = FALSE)
              }

              time_col <- spm_time_column(sspm_object)

              sspm_discrete_plot <- spm_plot_routine(smoothed_data, smoothed_var,
                                                     page, nrow, ncol, time_col, log)

              return(sspm_discrete_plot)
            }

          }
)

#' @export
#' @rdname spm_plot
setMethod("spm_plot",
          signature(sspm_object = "sspm_fit"),
          definition = function(sspm_object, smoothed_var = NULL,
                                page = "first", nrow = 2, ncol = 4) {

            smoothed_data <- spm_smoothed_data(sspm_object)

            if (!is.null(smoothed_var)) {

              if (!checkmate::test_subset(smoothed_var, names(smoothed_data))) {
                stop("`smoothed_var` must be a column of the smoothed data", call. = FALSE)
              }

              time_col <- spm_time_column(sspm_object)

              sspm_discrete_plot <- spm_plot_routine(smoothed_data, smoothed_var,
                                                     page, nrow, ncol, time_col)

            } else {

              preds <- spm_predict(sspm_object)$pred
              response <- spm_response(spm_formulas(sspm_object))
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
                ggplot2::facet_wrap(~.data[[spm_boundary_column(sspm_object)]])

            }

            return(sspm_discrete_plot)

          }
)

#' @export
#' @rdname spm_plot
setMethod("spm_plot_biomass",
          signature(sspm_object = "sspm_fit",
                    biomass = "character"),
          definition = function(sspm_object, biomass, use_sf,
                                nrow = 3, ncol = 3, page = 1, log = TRUE) {

            biomass_preds <- spm_predict_biomass(sspm_object, biomass)

            smoothed_data <- spm_smoothed_data(sspm_object) %>%
              dplyr::mutate(area = units::set_units(st_area(.data$geometry),
                                                    value = "km^2")) %>%
              dplyr::mutate(biomass = (.data[[biomass]] * as.numeric(area)))
            time_col <- spm_time_column(sspm_object)

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
          }
)

#' @export
#' @rdname spm_plot
setMethod("spm_plot_biomass",
          signature(sspm_object = "sspm_fit",
                    biomass = "sspm_dataset"),
          definition = function(sspm_object, biomass, biomass_var,
                                biomass_var_predict = NULL,
                                biomass_var_smooth = NULL,
                                biomass_var_origin = NULL,
                                nrow = 3, ncol = 3, page = 1) {

            if (is.null(biomass_var_predict)) {
              biomass_var_predict <- paste0(biomass_var, "_smooth")
            }

            if (is.null(biomass_var_smooth)) {
              biomass_var_smooth <- paste0(biomass_var, "_smooth")
            }

            if (is.null(biomass_var_origin)) {
              biomass_var_origin <- biomass_var
            }

            biomass_preds <- spm_predict_biomass(sspm_object, biomass_var_predict)
            boundary_col <- spm_boundary_column(sspm_object)
            time_col <- spm_time_column(sspm_object)

            biomass_smooth_summary <- spm_smoothed_data(biomass) %>%
              dplyr::mutate(area = as.numeric(units::set_units(st_area(.data$geometry),
                                                               value = "km^2")),
                            biomass = .data[[biomass_var_smooth]] * .data$area) %>%
              dplyr::group_by(.data[[boundary_col]], .data[[time_col]]) %>%
              dplyr::summarise(biomass_sum = sum(biomass)) %>%
              dplyr::mutate(!!time_col := as.numeric(as.character(.data[[time_col]])))

            biomass_preds <- biomass_preds %>%
              dplyr::group_by(.data[[boundary_col]], .data[[time_col]]) %>%
              dplyr::summarise(biomass_pred = sum(.data$biomass))

            biomass_actual <- spm_data(biomass) %>%

              dplyr::group_by(.data[[boundary_col]], .data$patch_id, .data[[time_col]]) %>%
              dplyr::summarise(biomass_mean = mean(.data[[biomass_var_origin]])) %>%

              dplyr::mutate(area = as.numeric(units::set_units(st_area(.data$geometry),
                                                               value = "km^2")),
                            biomass = .data$biomass_mean * .data$area) %>%

              dplyr::group_by(.data[[boundary_col]], .data[[time_col]]) %>%
              dplyr::summarise(biomass_sum = sum(.data$biomass)) %>%
              dplyr::mutate(!!time_col := as.numeric(as.character(.data[[time_col]])))

            biomass_plot <- biomass_preds %>%
              ggplot2::ggplot(ggplot2::aes(x = .data[[time_col]], y = .data$biomass_pred)) +
              ggplot2::geom_line(color = "red") +

              ggplot2::facet_wrap(~sfa, scales = "free") +
              ggplot2::geom_line(data = biomass_actual,
                                 ggplot2::aes(x = .data[[time_col]],
                                              y = .data$biomass_sum), col = "green") +

              ggplot2::geom_line(data = biomass_smooth_summary,
                                 ggplot2:: aes(x = .data[[time_col]],
                                               y = .data$biomass_sum), col = "blue") +
              ggplot2::scale_y_log10()

            return(biomass_plot)
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
