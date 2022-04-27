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
#' A ggplot2 plot object.
#'
#' @examples
#' \dontrun{
#' # To plot a boundary object and visualize patches/points
#' plot(sspm_boundary)
#' # To plot a dataset variable
#' plot(biomass_smooth, var = "weight_per_km2", log = FALSE)
#' plot(biomass_smooth, var = "weight_per_km2", use_sf = TRUE)
#' # To plot a fitted model
#' # Test-train plot
#' plot(sspm_model_fit, train_test = TRUE, scales = "free")
#' # Timeseries plot
#' plot(sspm_model_fit, log = T, scales = 'free')
#' plot(sspm_model_fit, log = T, use_sf = TRUE)
#' plot(sspm_model_fit, biomass = "weight_per_km2_borealis",  scales = "free")
#' plot(sspm_model_fit, biomass = "weight_per_km2_borealis", use_sf = TRUE)
#' plot(sspm_model_fit, biomass = "weight_per_km2_borealis",
#'      next_ts = TRUE, aggregate = TRUE, scales = "free", interval = T)
#' }
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
            boundary <- spm_boundary(x)

            if (checkmate::test_class(x, "sspm_discrete_boundary")) {

              patches <- spm_patches(x)
              points <- spm_points(x)

              sspm_discrete_plot <- ggplot2::ggplot() +
                ggplot2::geom_sf(data = patches,
                                 fill = NA, col = "#36454F") +
                ggplot2::geom_sf(data = boundaries,
                                 ggplot2::aes(col = .data[[boundary]]),
                                 fill = NA) +
                ggplot2::scale_color_viridis_d(boundary) +
                ggplot2::theme_light()

              if(!is.null(points)){
                sspm_discrete_plot <- sspm_discrete_plot +
                  ggplot2::geom_sf(data = points, col = "#6082B6")
              }

            } else if (checkmate::test_class(x, "sspm_boundary")) {

              sspm_discrete_plot <- ggplot2::ggplot() +
                ggplot2::geom_sf(data = boundaries,
                                 ggplot2::aes(fill = .data[[boundary]]),
                                 col = "#36454F") +
                ggplot2::scale_fill_viridis_d(boundary) +
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

            smoothed_data <- smoothed_data %>%
              dplyr::mutate(color = "Smoothed")
            time_col <- spm_time(x)

            if (is.null(var)) {

              cli::cli_alert_danger("`var` argument not specified.")
              cli::cli_alert_info("Please specify a variable to plot.")

            } else {

              assert_column(smoothed_data, var)

              time_col <- spm_time(x)

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

              sspm_discrete_plot <- plot_train_test(x, scales)

            } else {

              if (!is.null(biomass)){

                sspm_discrete_plot <- plot_biomass(x, biomass, biomass_origin,
                                                   aggregate, interval, use_sf,
                                                   page, nrow, ncol, log, scales,
                                                   next_ts)

              } else {

                if (aggregate) stop("productivity aggregate plotting is not enabled")

                sspm_discrete_plot <-
                  plot_productivity(x, aggregate, interval, use_sf, page, nrow,
                                    ncol, log, scales)

              }

            }

            return(sspm_discrete_plot)

          }
)
