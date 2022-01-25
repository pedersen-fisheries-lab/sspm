#' Plot `sspm` objects
#'
#' Plot methods for a range of sspm objects.
#'
#' @param x **\[sspm_...\]** An object from this package.
#' @param y NOT USED (from generic).
#' @param ... NOT USED (from generic).
#'
#' @param var **\[character\]** (For sspm_dataset) Variable to plot.
#'
#' @param train_test **\[logical\]** (For sspm_fit) Whether to plot a train/test
#'    pair plot.
#' @param biomass **\[character\]** (For sspm_fit) The biomass variable for
#'    predictions.
#' @param next_ts **\[logical\]** (For sspm_fit) Whether to plot a predictions
#'    for next timestep.
#' @param aggregate **\[logical\]** (For sspm_fit) For biomass predictions only,
#'    whether to aggregate the data to the boundary level. Default to FALSE.
#' @param interval **\[logical\]** (For sspm_fit) Whether to plot CI and Pi
#'    intervals.
#'
#' @param biomass_origin **\[character\]** Biomass variable to plot (from
#'    original dataset, optionnal).
#'
#' @param use_sf **\[logical\]** Whether to produce a spatial plot.
#' @param log **\[logical\]** For productivity, whether to plot log productivity,
#'    (default to FALSE) for others, whether to plot on a log scale (default to TRUE).
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
                                page = "first", nrow = 2, ncol = 2,
                                log = FALSE, scales = "fixed") {

            smoothed_data <- spm_smoothed_data(x)

            if (is.null(smoothed_data)){
              stop("Dataset doesn't have any smoothed data")
            }

            smoothed_data <- units::drop_units(smoothed_data) %>%
              dplyr::mutate(color = "Smoothed")
            time_col <- spm_time_column(x)

            if (is.null(var)) {

              cli::cli_alert_danger("`var` argument not specified.")
              cli::cli_alert_info("Please specify a variable to plot.")

            } else {

              if (!checkmate::test_subset(var, names(smoothed_data))) {
                stop("`var` must be a column of the smoothed data", call. = FALSE)
              }

              time_col <- spm_time_column(x)

              color_profile <- c("Smoothed" = "black")

              sspm_discrete_plot <-
                spm_plot_routine(smoothed_data = smoothed_data, var = var,
                                 use_sf = use_sf, page = page, nrow = nrow,
                                 ncol = ncol, time_col = time_col, log = log,
                                 scales = scales, color_profile = color_profile)

              return(sspm_discrete_plot)
            }

          }
)

#' @export
#' @rdname plot
setMethod("plot",
          signature(x = "sspm_fit",
                    y = "missing"),
          definition = function(x, y, ..., train_test = FALSE, biomass = NULL,
                                next_ts = FALSE, aggregate = FALSE,
                                interval = FALSE, biomass_origin = NULL,
                                use_sf = FALSE, page = "first", nrow = 2,
                                ncol = 2, log = FALSE, scales = "fixed") {

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

            } else {

              if (!is.null(biomass)){

                color_profile <- c("Predictions" = "red")

                boundary_col <- spm_boundary_column(x)
                patch_area_col <- spm_patches_area_column(spm_boundaries(x))

                checkmate::assert_character(biomass)

                biomass_preds <- predict(x, biomass = biomass,
                                         aggregate = aggregate,
                                         interval = interval) %>%
                  dplyr::mutate(color = "Predictions")

                if (next_ts) {

                  next_ts_label <- "Prediction (1 step \n ahead, NO CATCH)"

                  next_ts_preds <- predict(x, biomass = biomass,
                                           next_ts = next_ts,
                                           aggregate = aggregate) %>%
                    dplyr::mutate(color = next_ts_label)

                  time_col <- spm_time_column(x)
                  mext_ts_timestep <- max(unique(next_ts_preds[[time_col]]))-1

                  biomass_preds_previous <- biomass_preds %>%
                    dplyr::filter(.data[[time_col]] == mext_ts_timestep) %>%
                    dplyr::mutate(color = next_ts_label)

                  next_ts_preds <- next_ts_preds %>%
                    dplyr::bind_rows(biomass_preds_previous)

                  biomass_preds <- biomass_preds %>%
                    dplyr::bind_rows(next_ts_preds)

                  color_profile <-
                    c(color_profile, "Prediction (1 step \n ahead, NO CATCH)" =
                        "firebrick")
                }

                time_col <- spm_time_column(x)
                boundary_col <- spm_boundary_column(x)

                if (is.null(biomass_origin)){
                  # TODO check presence of column in data frame
                  biomass_origin <- biomass
                }

                biomass_actual <- spm_smoothed_data(x) %>%
                  dplyr::mutate(area =
                                  as.numeric(units::set_units(.data[[patch_area_col]],
                                                              value = "km^2")),
                                biomass = .data[[biomass_origin]] * .data$area)

                if (aggregate){
                  biomass_actual <-  biomass_actual %>%
                    dplyr::group_by(.data[[boundary_col]], .data[[time_col]]) %>%
                    dplyr::summarise(biomass = sum(.data$biomass)) %>%
                    dplyr::ungroup()
                }

                biomass_actual <- biomass_actual %>%
                  dplyr::mutate(color = "Smoothed")

                biomass_preds <- biomass_preds %>%
                  dplyr::bind_rows(biomass_actual)

                color_profile <- c(color_profile, "Smoothed" = "black")

                sspm_discrete_plot <-
                  spm_plot_routine(smoothed_data = biomass_preds, var = "biomass",
                                   use_sf = use_sf, page = page, nrow = nrow,
                                   ncol = ncol, time_col = time_col, log = log,
                                   scales = scales, color_profile = color_profile,
                                   aggregate = aggregate, interval = interval,
                                   boundary_col = boundary_col)

              } else {

                boundary_col <- spm_boundary_column(x)

                prod_preds <- predict(x, aggregate = aggregate,
                                      interval = interval) %>%
                  dplyr::mutate(color = "Predictions")

                actual <- spm_smoothed_data(x) %>%
                  dplyr::mutate(pred = exp(.data[[x@formula@response]])) %>%
                  dplyr::mutate(color = "Actual")

                prod_preds <- prod_preds %>%
                  dplyr::bind_rows(actual)

                time_col <- spm_time_column(x)

                color_profile <- c("Predictions" = "red",
                                   "Actual" = "black")

                sspm_discrete_plot <-
                  spm_plot_routine(smoothed_data = prod_preds, var = "pred",
                                   use_sf = use_sf, page = page, nrow = nrow,
                                   ncol = ncol, time_col = time_col, log = log,
                                   scales = scales, color_profile = color_profile,
                                   aggregate = aggregate, interval = interval,
                                   boundary_col = boundary_col)

              }

            }

            return(sspm_discrete_plot)

          }
)

# -------------------------------------------------------------------------

spm_plot_routine <- function(smoothed_data, var, use_sf, page, nrow, ncol,
                             time_col, log, scales, color_profile,
                             aggregate = FALSE, interval =  FALSE,
                             boundary_col) {

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
      ggplot2::geom_line(ggplot2::aes(x = .data[[time_col]],
                                      y = .data[[var]],
                                      color = .data$color)) +
      ggplot2::geom_point(ggplot2::aes(x = .data[[time_col]],
                                       y = .data[[var]],
                                       color = .data$color),
                          cex = 0.8) +
      ggplot2::labs(y = the_title) +
      ggplot2::theme_light() +
      ggplot2::scale_color_manual(values = color_profile) +
      ggplot2::labs(color = "Type",
                    x = "Timestep")

    if (aggregate){
      facet_by <- boundary_col
    } else {
      facet_by <- "patch_id"
    }

    if (interval) {

      if (log) {
        base_plot <- base_plot +
          ggplot2::geom_ribbon(
            ggplot2::aes(x = .data[[time_col]],
                         ymin = .data$CI_log_lower,
                         ymax = .data$CI_log_upper,
                         fill = .data$color), alpha = 0.5) +
          ggplot2::geom_ribbon(
            ggplot2::aes(x = .data[[time_col]],
                         ymin = .data$PI_log_lower,
                         ymax = .data$PI_log_upper,
                         fill = .data$color), alpha = 0.3) +
          ggplot2::scale_fill_manual(values = color_profile) +
          ggplot2::labs(fill = "Type")
      } else {
        base_plot <- base_plot +
          ggplot2::geom_ribbon(
            ggplot2::aes(x = .data[[time_col]],
                         ymin = .data$CI_lower,
                         ymax = .data$CI_upper,
                         fill = .data$color), alpha = 0.5) +
          ggplot2::geom_ribbon(
            ggplot2::aes(x = .data[[time_col]],
                         ymin = .data$PI_lower,
                         ymax = .data$PI_upper,
                         fill = .data$color), alpha = 0.3) +
          ggplot2::scale_fill_manual(values = color_profile) +
          ggplot2::labs(fill = "Type")
      }


    }

  }

  # Manage faceting + pagination
  if (is.character(page)) {

    if (page == "all") {

      facet_col_levels <- length(unique(smoothed_data[[facet_by]]))
      n_per_page <- nrow * ncol
      n_pages <- facet_col_levels %/% (n_per_page) +
        (facet_col_levels %% n_per_page >= 1)

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
