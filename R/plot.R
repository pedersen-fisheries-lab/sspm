#' Plot `sspm` objects
#'
#' Plot methods for a range of sspm objects.
#'
#' @param x **\[sspm_...\]** An object from this package.
#' @param y NOT USED (from generic).
#' @param ... NOT USED (from generic).
#'
#' @param var **\[character\]** (for sspm_dataset) Variable to plot.
#'
#' @param train_test **\[logical\]** (For sspm_fit) Whether to plot a train/test
#'      pair plot.
#' @param biomass **\[logical\]** (For sspm_fit) Whether to plot a biomass
#'      pair plot.
#'
#' @param biomass_var_predict **\[character\]** Biomass variable to plot (from
#'      predictions, OPTIONNAL).
#' @param biomass_var_smooth **\[character\]** Biomass variable to plot (smooth,
#'      optional).
#' @param biomass_var_origin **\[character\]** Biomass variable to plot (from
#'      original dataset, optionnal)
#'
#' @param use_sf **\[logical\]** Whether to produce a spatial plot.
#' @param log **\[logical\]** Whether to plot on a log scale, default to TRUE.
#' @inheritParams ggforce::facet_grid_paginate
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
          definition = function(x, y, ..., var = NULL, use_sf = FALSE,
                                page = "first", nrow = 2, ncol = 4,
                                log = FALSE, scales = "fixed") {

            smoothed_data <- spm_smoothed_data(x)
            time_col <- spm_time_column(x)

            if (is.null(var)) {

              cli::cli_alert_danger("`var` argument not specified.")
              cli::cli_alert_info("Please specify a variable to plot.")

            } else {

              if (!checkmate::test_subset(var, names(smoothed_data))) {
                stop("`var` must be a column of the smoothed data", call. = FALSE)
              }

              time_col <- spm_time_column(x)

              sspm_discrete_plot <- spm_plot_routine(smoothed_data, var, use_sf,
                                                     page, nrow, ncol, time_col,
                                                     log, scales)

              return(sspm_discrete_plot)
            }

          }
)

#' @export
#' @rdname plot
setMethod("plot",
          signature(x = "sspm_fit",
                    y = "missing"),
          definition = function(x, y, ..., train_test = FALSE, biomass = FALSE,
                                var = NULL, biomass_var_predict = NULL,
                                biomass_var_smooth = NULL, biomass_var_origin = NULL,
                                use_sf = FALSE, page = "first", nrow = 2, ncol = 4,
                                log = FALSE, scales = "fixed") {

            # If no biomass is provided, does a train/test plot (default)
            if (train_test){

              smoothed_data <- spm_smoothed_data(x)

              preds <- predict(x)$pred
              response <- spm_response(spm_formulas(x))

              smoothed_data_with_preds <- smoothed_data %>%
                dplyr::mutate(predicted = preds,
                              color = ifelse(.data$train_test, "TRAIN", "TEST"))

              # TRAIN / TEST pairs plot
              sspm_discrete_plot <-
                ggplot2::ggplot(data = smoothed_data_with_preds) +
                ggplot2::geom_point(ggplot2::aes(x = exp(.data[[response]]),
                                                 y = .data$predicted,
                                                 col = .data$color)) +
                ggplot2::theme_light() +
                ggplot2::labs(x = "actual") +
                ggplot2::scale_color_viridis_d("Set") +
                ggplot2::facet_wrap(~.data[[spm_boundary_column(x)]],
                                    scales = scales) +
                ggplot2::geom_abline(slope = 1, intercept = 0,
                                     lty = 2, size = 0.2)

            } else if (biomass){
              # If biomass is TRUE, do a biomass plot

              if (is.null(var)){
                stop("var cant be null")
              }

              biomass_preds <- predict(x, var)
              time_col <- spm_time_column(x)

              sspm_discrete_plot <-
                spm_plot_routine(smoothed_data = biomass_preds, var = "biomass",
                                 use_sf = use_sf, page = page, nrow = nrow,
                                 ncol = ncol, time_col = time_col, log = log,
                                 scales = scales)

            } else {

              prod_preds <- predict(x)
              time_col <- spm_time_column(x)

              sspm_discrete_plot <-
                spm_plot_routine(smoothed_data = prod_preds, var = "pred",
                                 use_sf = use_sf, page = page, nrow = nrow,
                                 ncol = ncol, time_col = time_col, log = log,
                                 scales = scales)

            }

            return(sspm_discrete_plot)

          }
)

# -------------------------------------------------------------------------

spm_plot_routine <- function(smoothed_data, var, use_sf,
                             page, nrow, ncol, time_col,
                             log, scales) {

  if (log) {
    smoothed_data[[var]] <- log(smoothed_data[[var]])
    the_title <- paste0(var, " (log)")
  } else {
    the_title <- var
  }

  if (use_sf){

    base_plot <- ggplot2::ggplot(data = smoothed_data) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data[[var]])) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::labs(fill = the_title) +
      ggplot2::theme_light()

    facet_by <- time_col

  } else {

    base_plot <- ggplot2::ggplot(data = smoothed_data) +
      ggplot2::geom_line(ggplot2::aes(x = .data[[time_col]], y = .data[[var]])) +
      ggplot2::labs(y = the_title) +
      ggplot2::theme_light()

    facet_by <- "patch_id"

  }

  # Manage facetting + pagination
  if (is.character(page)) {

    if (page == "all") {

      facet_col_levels <- length(unique(smoothed_data[[facet_by]]))
      n_per_page <- nrow * ncol
      n_pages <- facet_col_levels %/% (n_per_page) +
        (facet_col_levels %% n_per_page > 1)

      sspm_discrete_plot <- list()

      for (page_nb in seq_len(length.out = n_pages)) {

        sspm_discrete_plot[[page_nb]] <- base_plot +
          ggforce::facet_wrap_paginate(~ .data[[facet_by]],
                                       nrow = nrow, ncol = ncol,
                                       page = page_nb, scales = scales)

      }

    } else {

      sspm_discrete_plot <- base_plot +
        ggforce::facet_wrap_paginate(~ .data[[facet_by]],
                                     nrow = nrow, ncol = ncol,
                                     page = 1, scales = scales)

    }

  } else if (is.numeric(page)) {

    sspm_discrete_plot <- base_plot +
      ggforce::facet_wrap_paginate(~ .data[[facet_by]],
                                   nrow = nrow, ncol = ncol,
                                   page = page, scales = scales)

  }

  return(sspm_discrete_plot)

}

# biomass_smooth_summary <- spm_smoothed_data(biomass) %>%
#   dplyr::mutate(area = as.numeric(units::set_units(st_area(.data$geometry),
#                                                    value = "km^2")),
#                 biomass = .data[[biomass_var_smooth]] * .data$area) %>%
#   dplyr::group_by(.data[[boundary_col]], .data[[time_col]]) %>%
#   dplyr::summarise(biomass_sum = sum(biomass)) %>%
#   dplyr::mutate(!!time_col := as.numeric(as.character(.data[[time_col]])))

# ggplot2::geom_line(data = biomass_smooth_summary,
#                    ggplot2:: aes(x = .data[[time_col]],
#                                  y = .data$biomass_sum), col = "blue") +

# if (log) {
#
#   biomass_preds$biomass_pred <- log(biomass_preds$biomass)
#   smoothed_data$biomass <- log(smoothed_data$biomass)
#   the_title <- "Biomass (logged)"
#
# } else {
#
#   the_title <- "Biomass"
#
# }
#
# # SF OR NOT
# if (use_sf){
#
#   sspm_discrete_plot <- ggplot2::ggplot(data = biomass_preds) +
#     ggplot2::geom_sf(ggplot2::aes(fill = .data$biomass_pred)) +
#     ggforce::facet_wrap_paginate(~ .data[[time_col]],
#                                  nrow = nrow, ncol = ncol,
#                                  page = page) +
#     ggplot2::scale_fill_viridis_c() +
#     ggplot2::labs(fill = the_title)
#
# } else {
#
#   sspm_discrete_plot <- biomass_preds %>%
#     ggplot2::ggplot(ggplot2::aes(x = .data[[time_col]],
#                                  y = .data$biomass_pred)) +
#     ggplot2::geom_line(color = "red") +
#     ggforce::facet_wrap_paginate(~patch_id,
#                                  nrow = nrow, ncol = ncol,
#                                  page = page, scales = "free") +
#     ggplot2::geom_line(data = smoothed_data,
#                        ggplot2::aes(x = .data[[time_col]],
#                                     y = .data$biomass),
#                        color = "blue" )
# }

# else if (checkmate::test_class(biomass, "sspm_dataset")){
#
#   if (is.null(biomass_var_predict)) {
#     biomass_var_predict <- paste0(biomass_var, "_smooth")
#   }
#
#   if (is.null(biomass_var_smooth)) {
#     biomass_var_smooth <- paste0(biomass_var, "_smooth")
#   }
#
#   if (is.null(biomass_var_origin)) {
#     biomass_var_origin <- biomass_var
#   }
#
#   biomass_preds <- predict(x, biomass_var_predict)
#   boundary_col <- spm_boundary_column(x)
#   time_col <- spm_time_column(x)
#
#   biomass_preds <- biomass_preds %>%
#     dplyr::group_by(.data[[boundary_col]], .data[[time_col]]) %>%
#     dplyr::summarise(biomass_sum = sum(.data$biomass)) %>%
#     dplyr::mutate(type = "Predictions")
#
#   biomass_actual <- spm_data(biomass) %>%
#     dplyr::group_by(.data[[boundary_col]], .data$patch_id, .data[[time_col]]) %>%
#     dplyr::summarise(biomass_mean = mean(.data[[biomass_var_origin]])) %>%
#
#     dplyr::mutate(area = as.numeric(units::set_units(st_area(.data$geometry),
#                                                      value = "km^2")),
#                   biomass = .data$biomass_mean * .data$area) %>%
#
#     dplyr::group_by(.data[[boundary_col]], .data[[time_col]]) %>%
#     dplyr::summarise(biomass_sum = sum(.data$biomass)) %>%
#     dplyr::mutate(!!time_col := as.numeric(as.character(.data[[time_col]])))  %>%
#     dplyr::mutate(type = "Actual")
#
#   sspm_discrete_plot <- biomass_preds %>%
#     dplyr::bind_rows(biomass_actual) %>%
#     dplyr::filter(.data$year_f %in% c(2006:2020)) %>%
#     ggplot2::ggplot() +
#     ggplot2::facet_wrap(~sfa, scales = "free") +
#     ggplot2::geom_line(ggplot2::aes(x = .data[[time_col]],
#                                     y = .data$biomass_sum,
#                                     color = .data$type,
#                                     linetype = .data$type)) +
#     ggplot2::scale_y_log10()

# smoothed_data <- spm_smoothed_data(x) %>%
#   dplyr::mutate(area = units::set_units(st_area(.data$geometry),
#                                         value = "km^2")) %>%
#   dplyr::mutate(biomass = (.data[[biomass]] * as.numeric(.data$area)))
# time_col <- spm_time_column(x)
