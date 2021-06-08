#' Plot various `sspm` objects
#'
#' Plot methods for a range of sspm objects.
#'
#' @param x An object from this package.
#' @param smoothed_var TODO
#' @param page TODO
#' @param nrow TODO
#' @param ncol TODO
#'
#' @return
#' N/A
#'
#' @name plot.sspm
#' @rdname plot-sspm

NULL

#' @rdname plot-sspm
#' @export
setMethod("plot",
          signature(x = "sspm_boundary"),
          definition = function(x) {

            boundaries <- x@boundaries
            b_col <- x@boundary_column

            if (checkmate::test_class(x, "sspm_discrete_boundary")){

              patches <- x@patches
              points <- x@points

              sspm_discrete_plot <- ggplot2::ggplot() +
                ggplot2::geom_sf(data = patches,
                                 fill = NA, col = "#36454F") +
                ggplot2::geom_sf(data = points,
                                 col ="#6082B6") +
                ggplot2::geom_sf(data = boundaries,
                                 ggplot2::aes(col = .data[[b_col]]),
                                 fill = NA) +
                ggplot2::scale_color_viridis_d(b_col) +
                ggplot2::theme_light()

            } else if(checkmate::test_class(x, "sspm_boundary")){

              sspm_discrete_plot <- ggplot2::ggplot() +
                ggplot2::geom_sf(data = boundaries,
                                 ggplot2::aes(fill = .data[[b_col]]),
                                 col = "#36454F") +
                ggplot2::scale_fill_viridis_d(b_col) +
                ggplot2::theme_light()

            }

            return(sspm_discrete_plot)

          }
)

#' @rdname plot-sspm
#' @export
setMethod("plot",
          signature(x = "sspm_data"),
          definition = function(x, smoothed_var = NULL,
                                page = "first", nrow = 2, ncol = 4) {

            smoothed_data <- spm_smoothed_data(x)

            if(is.null(smoothed_data)){

              cli::cli_alert_warning("Data object not smoothed, nothing to plot")

            } else {

              if(is.null(smoothed_var)){

                cli::cli_alert_warning("Please specify which variable to plot")

              } else {

                # TODO add check for smoothed_var to be in smoothed_data

                time_col <- spm_time_column(x)

                if(is.character(page)){

                  if (page == "all"){

                    time_col_levels <- length(unique(smoothed_data[[time_col]]))
                    n_per_page <- nrow*ncol
                    n_pages <- time_col_levels %/% (n_per_page) +
                      (time_col_levels %% n_per_page > 1)

                    sspm_discrete_plot <- list()

                    for (page_nb in seq_len(length.out = n_pages)){

                      sspm_discrete_plot[[page_nb]] <-
                        ggplot2::ggplot(data = smoothed_data) +
                        ggplot2::geom_sf(ggplot2::aes(fill=.data[[smoothed_var]])) +
                        ggforce::facet_wrap_paginate(~.data[[time_col]],
                                                     nrow = nrow, ncol = ncol,
                                                     page = page_nb) +
                        ggplot2::scale_fill_viridis_c()

                    }

                  } else {

                    sspm_discrete_plot <- ggplot2::ggplot(data = smoothed_data) +
                      ggplot2::geom_sf(ggplot2::aes(fill=.data[[smoothed_var]])) +
                      ggforce::facet_wrap_paginate(~.data[[time_col]],
                                                   nrow = nrow, ncol = ncol,
                                                   page = 1) +
                      ggplot2::scale_fill_viridis_c()

                  }

                } else if(is.numeric(page)){

                  browser()

                  sspm_discrete_plot <- ggplot2::ggplot(data = smoothed_data) +
                    ggplot2::geom_sf(ggplot2::aes(fill=.data[[smoothed_var]])) +
                    ggforce::facet_wrap_paginate(~.data[[time_col]],
                                                 nrow = nrow, ncol = ncol,
                                                 page = page) +
                    ggplot2::scale_fill_viridis_c()

                }
                return(sspm_discrete_plot)
              }
            }
          }
)
