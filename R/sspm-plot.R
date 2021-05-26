#' Plot a discrete `sspm` model object
#'
#' This will plot the patches and points of a `sspm_discrete` object.
#'
#' @param x **\[sspm_discrete\]** An object of class
#'     [sspm_discrete][sspm_discrete-class].
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
          signature(x = "sspm"),
          definition = function(x) {
            cli::cli_alert_danger(" This model is not discretized and cannot be plotted")
          }
)

#' @rdname plot-sspm
#' @export
setMethod("plot",
          signature(x = "sspm_discrete"),
          definition = function(x, smoothed_var = NULL,
                                page = "first", nrow = 2, ncol = 4) {

            smoothed_dataset <- spm_smoothed_data(x)
            smoothed_data <- spm_data(smoothed_dataset)

            if(is.null(smoothed_var) | is.null(smoothed_data)){

              sspm_discrete_plot <- ggplot2::ggplot() +
                ggplot2::geom_sf(data = spm_patches(x),
                                 fill = NA, col = "#8A9A5B") +
                ggplot2::geom_sf(data = spm_boundaries(x),
                                 fill = NA, col = "#36454F") +
                ggplot2::geom_sf(data = spm_points(x),
                                 col ="#6082B6") +
                ggplot2::theme_light()

            } else {

              # TODO add check for smoothed_var to be in smoothed_data

              time_col <- spm_time_column(smoothed_dataset)

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

                sspm_discrete_plot <- ggplot2::ggplot(data = smoothed_data) +
                  ggplot2::geom_sf(ggplot2::aes(fill=.data[[smoothed_var]])) +
                  ggforce::facet_wrap_paginate(~.data[[time_col]],
                                               nrow = nrow, ncol = ncol,
                                               page = page) +
                  ggplot2::scale_fill_viridis_c()

              }
            }

            return(sspm_discrete_plot)

          }
)
